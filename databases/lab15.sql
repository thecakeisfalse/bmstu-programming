USE master;
GO
IF DB_ID (N'DB15_1') IS NOT NULL
ALTER DATABASE DB15_1 SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
DROP DATABASE DB15_1;
GO

CREATE DATABASE DB15_1
ON PRIMARY
(
  NAME = lab15_1dat,
  FILENAME = '/var/opt/mssql/data/lab15_1.mdf',
  SIZE = 10,
  MAXSIZE = UNLIMITED,
  FILEGROWTH = 5%
)
LOG ON
(
  NAME = lab15_1log,
  FILENAME = '/var/opt/mssql/data/lab15_1log.ldf',
  SIZE = 5MB,
  MAXSIZE = 25MB,
  FILEGROWTH = 5MB
)
GO
IF DB_ID (N'DB15_2') IS NOT NULL
ALTER DATABASE DB15_2 SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
DROP DATABASE DB15_2;
GO

CREATE DATABASE DB15_2
ON PRIMARY
(
  NAME = lab15_2dat,
  FILENAME = '/var/opt/mssql/data/lab15_2.mdf',
  SIZE = 10,
  MAXSIZE = UNLIMITED,
  FILEGROWTH = 5%
)
LOG ON
(
  NAME = lab15_2log,
  FILENAME = '/var/opt/mssql/data/lab15_2log.ldf',
  SIZE = 5MB,
  MAXSIZE = 25MB,
  FILEGROWTH = 5MB
)
GO

---

USE DB15_1;

DROP TABLE IF EXISTS REQUEST;
GO

CREATE TABLE REQUEST(
    id int PRIMARY KEY,
    max_tokens int NOT NULL CHECK(max_tokens > 0),
    prompt nvarchar(100) NOT NULL CHECK(len(prompt) > 0)
);
GO

USE DB15_2;


DROP TABLE IF EXISTS RESPONSE;
GO

CREATE TABLE RESPONSE(
    id int IDENTITY(1,1) PRIMARY KEY,
    generated_tokens int NOT NULL CHECK(generated_tokens > 0),
    content nvarchar(100) NOT NULL CHECK(len(content) > 0),
    request_id int unique NOT NULL
);

GO

---

DROP VIEW IF EXISTS SectionRR_View;
GO

CREATE VIEW SectionRR_VIEW AS
    SELECT o.id, o.max_tokens, o.prompt, t.generated_tokens, t.content
    FROM DB15_1.dbo.REQUEST as o, DB15_2.dbo.RESPONSE as t
    WHERE o.id = t.request_id;
GO

INSERT INTO DB15_1.dbo.REQUEST(id, max_tokens, prompt) VALUES
(1, 1000, 'What is an index?'),
(2, 3300, 'What is the difference between Deepseek and ChatGPT?'),
(3, 1013, 'prompt');
GO

INSERT INTO DB15_2.dbo.RESPONSE(generated_tokens, content, request_id) VALUES
(10, 'i don''t know', 2),
(30, 'don''t know either', 1),
(40, 'reponse', 3);
GO

SELECT * FROM SectionRR_VIEW ORDER BY id;
SELECT * FROM DB15_1.dbo.REQUEST;
SELECT * FROM DB15_2.dbo.RESPONSE;
GO

USE DB15_1;

DROP TRIGGER IF EXISTS ReqUpdate;
DROP TRIGGER IF EXISTS ReqDelete;
GO

CREATE TRIGGER ReqDelete ON REQUEST
AFTER DELETE
AS
BEGIN
    DELETE o
    FROM DB15_2.dbo.RESPONSE as o INNER JOIN deleted AS u ON o.request_id = u.id;
END
GO

CREATE TRIGGER ReqUpdate ON REQUEST
AFTER UPDATE
AS
BEGIN
    IF UPDATE(id)
        BEGIN
            RAISERROR('ID can''t be updated', 16, 1);
            ROLLBACK TRANSACTION;
            RETURN;
        END
END
GO

USE DB15_2;

DROP TRIGGER IF EXISTS RespUpdate;
DROP TRIGGER IF EXISTS RespInsert;
GO

CREATE TRIGGER RespInsert ON RESPONSE
AFTER INSERT
AS
BEGIN
    IF EXISTS (
        SELECT 1
        FROM DB15_1.dbo.REQUEST as o RIGHT JOIN inserted as u ON u.request_id = o.id
        WHERE o.id is NULL
    )
        BEGIN
            RAISERROR('One of requests wasn''t found', 16, 1);
            ROLLBACK TRANSACTION;
            RETURN;
        END
END
GO

CREATE TRIGGER RespUpdate ON RESPONSE
AFTER UPDATE
AS
BEGIN
    IF UPDATE(request_id) OR UPDATE(id)
        BEGIN
            RAISERROR('ID can''t be updated', 16, 1);
            ROLLBACK TRANSACTION;
            RETURN;
        END
END
GO

INSERT INTO DB15_1.dbo.REQUEST VALUES
(4, 50, 'prompt 2');

UPDATE DB15_1.dbo.REQUEST
SET max_tokens = 60 WHERE id = 3;

DELETE DB15_1.dbo.REQUEST WHERE id = 2;

SELECT * FROM DB15_1.dbo.REQUEST;
SELECT * FROM DB15_2.dbo.RESPONSE;
GO

INSERT INTO DB15_2.dbo.RESPONSE(content, generated_tokens, request_id) VALUES
-- ('content', 30, 5),
('content', 30, 4);
GO

UPDATE DB15_2.dbo.RESPONSE
SET generated_tokens = 10000 WHERE request_id = 1;

DELETE FROM DB15_2.dbo.RESPONSE
WHERE id = 3;

SELECT * FROM DB15_1.dbo.REQUEST;
SELECT * FROM DB15_2.dbo.RESPONSE;
SELECT * FROM SectionRR_VIEW;
GO
