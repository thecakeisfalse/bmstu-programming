USE master;
GO
IF DB_ID (N'LLM_DB6') IS NOT NULL
ALTER DATABASE LLM_DB6 SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
DROP DATABASE LLM_DB6;
GO

CREATE DATABASE LLM_DB6
ON PRIMARY
(
  NAME = lab6dat,
  FILENAME = '/var/opt/mssql/data/lab6.mdf',
  SIZE = 10,
  MAXSIZE = UNLIMITED,
  FILEGROWTH = 5%
)
LOG ON
(
  NAME = lab6log,
  FILENAME = '/var/opt/mssql/data/lab6log.ldf',
  SIZE = 5MB,
  MAXSIZE = 25MB,
  FILEGROWTH = 5MB
)
GO

--

USE LLM_DB6;
GO

IF OBJECT_ID(N'USERS') IS NOT NULL
DROP TABLE USERS;
GO

CREATE TABLE USERS(
  id int IDENTITY(1,1) PRIMARY KEY,
  email nvarchar(100) UNIQUE NOT NULL
        CHECK(LEN(email) > 6 AND email LIKE '%_@__%.__%'),
  phone_number char(11) NOT NULL CHECK(LEN(phone_number) = 11),
  registration_date date NOT NULL DEFAULT(GETDATE())
);

INSERT INTO USERS(email, phone_number, registration_date) VALUES
('email@mail.ru', '88005553535', '2022-01-31'),
('email@gmail.com', '79003241234', '2025-10-04');
GO

SELECT @@IDENTITY as UserID;
SELECT SCOPE_IDENTITY() as UserID_scope;
SELECT IDENT_CURRENT('USERS') as UserId_current;

INSERT INTO USERS(email, phone_number, registration_date) VALUES
('user@yandex.ru', '74112341422', '2025-05-12'),
('user2@zmail.su', '73241234123', '2024-11-13');
GO

SELECT @@IDENTITY as UserID;
SELECT SCOPE_IDENTITY() as UserID_scope;
SELECT IDENT_CURRENT('USERS') as UserId_current;

SELECT * FROM USERS;

---

IF OBJECT_ID(N'SUBSCRIPTION') IS NOT NULL
DROP TABLE SUBSCRIPTION;
GO

CREATE TABLE SUBSCRIPTION(
  id UNIQUEIDENTIFIER PRIMARY KEY DEFAULT(NEWID()),
  name nvarchar(100) UNIQUE NOT NULL CHECK(LEN(name) > 1),
  price FLOAT NOT NULL CHECK(price > 0),
  monthly_token_limit INT NOT NULL CHECK(monthly_token_limit > 0),
);

INSERT INTO SUBSCRIPTION(name,price,monthly_token_limit) VALUES
('Base', 100, 1000),
('Pro', 1000, 5000);
GO


SELECT * FROM SUBSCRIPTION;
GO

---


DROP SEQUENCE IF EXISTS ModelSeq;
GO

CREATE SEQUENCE ModelSeq 
    START WITH 1
    INCREMENT BY 1
    MAXVALUE 100;
GO

IF OBJECT_ID(N'MODEL') IS NOT NULL
DROP TABLE MODEL;
GO

CREATE TABLE MODEL(
  id int PRIMARY KEY,
  name nvarchar(100) NOT NULL,
  version nvarchar(100) NOT NULL,
  CONSTRAINT UQ_name_version UNIQUE(name, version),
);
GO

INSERT INTO MODEL(id, name, version) VALUES
(NEXT VALUE FOR ModelSeq, 'DeepSeek', 'v1'),
(NEXT VALUE FOR ModelSeq, 'DeepSeek', 'v2'),
(NEXT VALUE FOR ModelSeq, 'ChatGPT', 'o1'),
(NEXT VALUE FOR ModelSeq, 'ChatGPT', '4'),
(NEXT VALUE FOR ModelSeq, 'ChatGPT', 'o2-mini');
GO

SELECT * FROM MODEL;

---

IF OBJECT_ID(N'REQUEST') IS NOT NULL
DROP TABLE REQUEST;
GO

CREATE TABLE REQUEST(
    id int PRIMARY KEY,
    max_tokens int NOT NULL CHECK(max_tokens > 0),
    prompt nvarchar(100) NOT NULL CHECK(len(prompt) > 0)
);
GO

IF OBJECT_ID(N'RESPONSE') IS NOT NULL
DROP TABLE RESPONSE;
GO

CREATE TABLE RESPONSE(
    id int IDENTITY(1,1) PRIMARY KEY,
    generated_tokens int NOT NULL CHECK(generated_tokens > 0),
    content nvarchar(100) NOT NULL CHECK(len(content) > 0),
    request_id int DEFAULT(0) foreign key references REQUEST(id)
    -- ON DELETE SET NULL
    -- ON DELETE NO ACTION
    -- ON DELETE SET DEFAULT
    ON DELETE CASCADE
);
GO

INSERT INTO REQUEST(id, max_tokens, prompt) VALUES
(1, 1000, 'What is index?'),
(2, 3300, 'What is the difference between Deepseek and ChatGPT?');
-- (0, 9999, 'default value');
SELECT * FROM REQUEST;
GO

INSERT INTO RESPONSE(generated_tokens, content, request_id) VALUES
(10, 'i don''t know', 2),
(30, 'don''t know either', 1);
SELECT * FROM RESPONSE;
GO

DELETE FROM REQUEST WHERE id='1';


SELECT * FROM REQUEST;
SELECT * FROM RESPONSE;
GO
