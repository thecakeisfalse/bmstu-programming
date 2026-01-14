USE master;
GO
IF DB_ID (N'LLM_DB9') IS NOT NULL
ALTER DATABASE LLM_DB9 SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
DROP DATABASE LLM_DB9;
GO

CREATE DATABASE LLM_DB9
ON PRIMARY
(
  NAME = lab9dat,
  FILENAME = '/var/opt/mssql/data/lab9.mdf',
  SIZE = 10,
  MAXSIZE = UNLIMITED,
  FILEGROWTH = 5%
)
LOG ON
(
  NAME = lab9log,
  FILENAME = '/var/opt/mssql/data/lab9log.ldf',
  SIZE = 5MB,
  MAXSIZE = 25MB,
  FILEGROWTH = 5MB
)
GO

---

USE LLM_DB9;
GO

DROP TABLE IF EXISTS REQUEST;
GO

CREATE TABLE REQUEST(
    id int PRIMARY KEY,
    max_tokens int NOT NULL CHECK(max_tokens > 0),
    prompt nvarchar(100) NOT NULL CHECK(len(prompt) > 0)
);
GO

DROP TABLE IF EXISTS RESPONSE;
GO

CREATE TABLE RESPONSE(
    id int primary key foreign key references REQUEST(id)
    ON DELETE CASCADE,
    generated_tokens int NOT NULL CHECK(generated_tokens > 0),
    content nvarchar(100) NOT NULL CHECK(len(content) > 0)
);
GO

INSERT INTO REQUEST(id, max_tokens, prompt) VALUES
(1, 1000, 'What is an index?'),
(2, 3300, 'What is the difference between Deepseek and ChatGPT?');
GO

INSERT INTO RESPONSE(generated_tokens, content, id) VALUES
(10, 'i don''t know', 2),
(30, 'don''t know either', 1);
GO

SELECT * FROM REQUEST;
SELECT * FROM RESPONSE;

GO

DROP TRIGGER IF EXISTS ReqInsert;
DROP TRIGGER IF EXISTS ReqDelete;
DROP TRIGGER IF EXISTS ReqUpdate;
GO

--

CREATE TRIGGER ReqInsert on REQUEST
INSTEAD OF INSERT
AS
BEGIN
    PRINT('Insert trigger')
    INSERT INTO REQUEST(id, max_tokens, prompt)
    SELECT id, max_tokens, prompt FROM Inserted
END
GO

SELECT * FROM REQUEST;

INSERT INTO REQUEST VALUES
(3, 10012, 'prompt'),
(4, 10213, 'prompt2');

SELECT * FROM REQUEST;

GO

--

CREATE TRIGGER ReqDelete ON REQUEST
INSTEAD OF DELETE
AS
BEGIN
    PRINT('Delete Trigger')
    DELETE req
    FROM REQUEST AS req INNER JOIN deleted AS d ON req.id = d.id;
END
GO

DELETE REQUEST WHERE max_tokens > 4000;
SELECT * FROM REQUEST;

GO

--

CREATE TRIGGER ReqUpdate ON REQUEST
INSTEAD OF UPDATE
AS
BEGIN
  PRINT('Update Trigger')
  IF UPDATE(id)
    BEGIN
        RAISERROR('ID can''t be updated', 16, 1);
        ROLLBACK TRANSACTION;
        RETURN;
    END
  ELSE
    UPDATE REQUEST
    SET max_tokens = u.max_tokens, prompt = u.prompt
    FROM REQUEST as req INNER JOIN inserted AS u ON req.id = u.id;
END

GO

-- UPDATE REQUEST SET id = 7 WHERE max_tokens = 1000;
UPDATE REQUEST SET max_tokens = 5000 WHERE max_tokens < 2000;

SELECT * FROM REQUEST;

GO

---

DROP VIEW IF EXISTS RR_VIEW;
GO

CREATE VIEW RR_VIEW AS
SELECT req.id, req.prompt, req.max_tokens, resp.content, resp.generated_tokens
FROM REQUEST as req INNER JOIN RESPONSE as resp ON req.id = resp.id
GO

DROP TRIGGER IF EXISTS RRInsert;
DROP TRIGGER IF EXISTS RRDelete;
DROP TRIGGER IF EXISTS RRUpdate;
GO

SELECT * FROM RR_VIEW;
GO

--

CREATE TRIGGER RRDelete ON RR_VIEW
INSTEAD OF DELETE
AS
BEGIN
    PRINT('Delete View Trigger')
    DELETE req
    FROM REQUEST AS req INNER JOIN deleted AS d ON req.id = d.id;
END
GO

-- DELETE RR_VIEW WHERE id = 1;
SELECT * FROM RR_VIEW;
GO

--

CREATE TRIGGER RRUpdate ON RR_VIEW
INSTEAD OF UPDATE
AS
BEGIN
    PRINT('Update View Trigger')

    IF UPDATE(id)
      BEGIN
        RAISERROR('ID can''t be updated', 16, 1);
        ROLLBACK TRANSACTION;
        RETURN;
      END
    ELSE
      BEGIN
        UPDATE REQUEST
        SET max_tokens = u.max_tokens, prompt = u.prompt
        FROM REQUEST as req INNER JOIN inserted AS u ON req.id = u.id;

        UPDATE RESPONSE
        SET content = u.content, generated_tokens = u.generated_tokens
        FROM RESPONSE as resp INNER JOIN inserted AS u ON resp.id = u.id;
      END
END
GO

-- UPDATE RR_VIEW SET id = 12 WHERE id = 1;
UPDATE RR_VIEW SET generated_tokens = 1 WHERE id = 2;
SELECT * FROM RR_VIEW;
GO

--

CREATE TRIGGER RRInsert ON RR_VIEW
INSTEAD OF INSERT
AS
BEGIN
    PRINT('Insert View Trigger')

    INSERT INTO REQUEST(id, max_tokens, prompt)
    SELECT id, max_tokens, prompt FROM inserted;

    INSERT INTO RESPONSE(id, content, generated_tokens)
    SELECT id, content, generated_tokens FROM inserted
    WHERE content IS NOT NULL AND generated_tokens IS NOT NULL;
END
GO

SELECT * FROM REQUEST;
SELECT * FROM RESPONSE;
SELECT * FROM RR_VIEW;

INSERT INTO RR_VIEW(id, prompt, max_tokens, content, generated_tokens) VALUES
(4, 'prompt 2', 1000, 'content 2', 200);

INSERT INTO RR_VIEW(id, prompt, max_tokens) VALUES
(5, 'prompt 2', 1000);


SELECT * FROM REQUEST;
SELECT * FROM RESPONSE;
SELECT * FROM RR_VIEW;
GO
