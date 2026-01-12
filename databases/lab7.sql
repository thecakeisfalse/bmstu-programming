USE master;
GO
IF DB_ID (N'LLM_DB7') IS NOT NULL
ALTER DATABASE LLM_DB7 SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
DROP DATABASE LLM_DB7;
GO

CREATE DATABASE LLM_DB7
ON PRIMARY
(
  NAME = lab7dat,
  FILENAME = '/var/opt/mssql/data/lab7.mdf',
  SIZE = 10,
  MAXSIZE = UNLIMITED,
  FILEGROWTH = 5%
)
LOG ON
(
  NAME = lab7log,
  FILENAME = '/var/opt/mssql/data/lab7log.ldf',
  SIZE = 5MB,
  MAXSIZE = 25MB,
  FILEGROWTH = 5MB
)
GO

--

USE LLM_DB7;
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
('email@gmail.com', '79003241234', '2025-10-04'),
('user@yandex.ru', '74112341422', '2025-05-12'),
('user2@mail.su', '73241234123', '2024-11-13');
GO

IF OBJECT_ID(N'USERS_VIEW') IS NOT NULL
DROP VIEW USERS_VIEW;
GO

CREATE VIEW USERS_VIEW AS
SELECT u.email, u.phone_number, u.registration_date
FROM USERS as u
WHERE u.email LIKE '%_@mail.__%';
GO

-- INSERT INTO USERS(email, phone_number) VALUES
-- ('lol@mail.ru', '12312312312');
-- INSERT INTO USERS_VIEW(email, phone_number) VALUES
-- ('lol2@mail.ru', '12312312312');
-- GO

SELECT * FROM USERS;
SELECT * FROM USERS_VIEW;

---

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
    id int IDENTITY(1,1) PRIMARY KEY,
    generated_tokens int NOT NULL CHECK(generated_tokens > 0),
    content nvarchar(100) NOT NULL CHECK(len(content) > 0),
    request_id int unique DEFAULT(0) foreign key references REQUEST(id)
    ON DELETE CASCADE
);
GO

INSERT INTO REQUEST(id, max_tokens, prompt) VALUES
(1, 1000, 'What is an index?'),
(2, 3300, 'What is the difference between Deepseek and ChatGPT?');
GO

INSERT INTO RESPONSE(generated_tokens, content, request_id) VALUES
(10, 'i don''t know', 2),
(30, 'don''t know either', 1);
GO

DROP VIEW IF EXISTS RR_VIEW;
GO

CREATE VIEW RR_VIEW AS
SELECT req.id, req.prompt, req.max_tokens, resp.content, resp.generated_tokens
FROM REQUEST as req INNER JOIN RESPONSE as resp ON req.id = resp.request_id
GO

-- INSERT INTO REQUEST(id, max_tokens, prompt) VALUES
-- (3, 1000, 'prompt');
-- INSERT INTO RESPONSE(generated_tokens, content, request_id) VALUES
-- (100, 'content', 3);
-- INSERT INTO RR_VIEW(id, prompt, max_tokens, content, generated_tokens) VALUES
-- (4, 'prompt 2', 1000, 'content 2', 200);
-- GO

SELECT * FROM REQUEST;
SELECT * FROM RESPONSE;
SELECT * FROM RR_VIEW;

---

DROP TABLE IF EXISTS USERS;
GO

CREATE TABLE USERS(
  id int IDENTITY(1,1) PRIMARY KEY,
  email nvarchar(100) UNIQUE NOT NULL
        CHECK(LEN(email) > 6 AND email LIKE '%_@__%.__%'),
  phone_number char(11) NOT NULL CHECK(LEN(phone_number) = 11),
  registration_date date NOT NULL DEFAULT(GETDATE()),
  name nvarchar(100) NOT NULL CHECK(LEN(name) > 0),
  surname nvarchar(100) NOT NULL CHECK(LEN(surname) > 0)
);

INSERT INTO USERS(email, phone_number, name, surname) VALUES
('gordejka@179.ru', '89xxxxxxxxx', 'Ivan', 'Gordeev'),
('user@gmail.com', '89123456789', 'Pavel', 'Pugachev'),
('email@mail.ru', '88005553535', 'Ivan', 'Maximov'),
('email@mail.com', '79003241234', 'First name', 'Surname'),
('user2@mail.su', '73241234123', 'Ivan', 'Ivanov');
GO

DROP INDEX IF EXISTS USER_NAME_INDEX ON USERS;

CREATE INDEX USERS_NAME_INDEX
ON USERS(surname, name)
INCLUDE (email, phone_number, registration_date)
GO

SELECT surname, name, email, phone_number, registration_date
FROM USERS
WHERE name = 'Ivan' AND surname = 'Gordeev';

---

DROP VIEW IF EXISTS USERS_INDEX_VIEW;
GO

CREATE VIEW USERS_INDEX_VIEW
WITH SCHEMABINDING AS
SELECT email, phone_number, registration_date, name, surname
FROM dbo.USERS
WHERE email LIKE '%_@mail.__%';
GO

DROP INDEX IF EXISTS USERS_EMAIL_INDEX ON USERS_INDEX_VIEW;

CREATE UNIQUE CLUSTERED INDEX USERS_EMAIL_INDEX
ON USERS_INDEX_VIEW(email)
GO

SELECT * FROM USERS_INDEX_VIEW;

GO
