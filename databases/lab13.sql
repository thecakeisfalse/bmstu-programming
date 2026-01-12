USE master;
GO
IF DB_ID (N'DB13_1') IS NOT NULL
ALTER DATABASE DB13_1 SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
DROP DATABASE DB13_1;
GO

CREATE DATABASE DB13_1
ON PRIMARY
(
  NAME = lab13_1dat,
  FILENAME = '/var/opt/mssql/data/lab13_1.mdf',
  SIZE = 10,
  MAXSIZE = UNLIMITED,
  FILEGROWTH = 5%
)
LOG ON
(
  NAME = lab13_1log,
  FILENAME = '/var/opt/mssql/data/lab13_1log.ldf',
  SIZE = 5MB,
  MAXSIZE = 25MB,
  FILEGROWTH = 5MB
)
GO
IF DB_ID (N'DB13_2') IS NOT NULL
DROP DATABASE DB13_2;
GO

CREATE DATABASE DB13_2
ON PRIMARY
(
  NAME = lab13_2dat,
  FILENAME = '/var/opt/mssql/data/lab13_2.mdf',
  SIZE = 10,
  MAXSIZE = UNLIMITED,
  FILEGROWTH = 5%
)
LOG ON
(
  NAME = lab13_2log,
  FILENAME = '/var/opt/mssql/data/lab13_2log.ldf',
  SIZE = 5MB,
  MAXSIZE = 25MB,
  FILEGROWTH = 5MB
)
GO

---

USE DB13_1;
GO

DROP TABLE IF EXISTS REQUEST;
GO

CREATE TABLE REQUEST(
    id int PRIMARY KEY CHECK(id < 10),
    max_tokens int NOT NULL CHECK(max_tokens > 0),
    prompt nvarchar(100) NOT NULL CHECK(len(prompt) > 0)
);
GO

USE DB13_2;
GO

DROP TABLE IF EXISTS REQUEST;
GO

CREATE TABLE REQUEST(
    id int PRIMARY KEY CHECK(id >= 10),
    max_tokens int NOT NULL CHECK(max_tokens > 0),
    prompt nvarchar(100) NOT NULL CHECK(len(prompt) > 0)
);
GO

DROP VIEW IF EXISTS SectionRequest_View;
GO

CREATE VIEW SectionRequest_View AS
    SELECT * FROM DB13_1.dbo.REQUEST
    UNION ALL
    SELECT * FROM DB13_2.dbo.REQUEST;
GO

INSERT INTO SectionRequest_View(id, max_tokens, prompt) VALUES
(1, 100, 'prompt 1'),
(12, 100, 'prompt 2'),
(3, 201, 'not a prompt');
GO

SELECT * FROM SectionRequest_View ORDER BY id;
SELECT * FROM DB13_1.dbo.REQUEST;
SELECT * FROM DB13_2.dbo.REQUEST;
GO

UPDATE SectionRequest_View
SET max_tokens = 1000 WHERE id = 12;

-- UPDATE SectionRequest_View
-- SET id = 5 WHERE id = 12;

SELECT * FROM SectionRequest_View ORDER BY id;
SELECT * FROM DB13_1.dbo.REQUEST;
SELECT * FROM DB13_2.dbo.REQUEST;
GO

DELETE SectionRequest_View
WHERE NOT (max_tokens BETWEEN 150 AND 250);

SELECT * FROM SectionRequest_View ORDER BY id;
SELECT * FROM DB13_1.dbo.REQUEST;
SELECT * FROM DB13_2.dbo.REQUEST;
GO
