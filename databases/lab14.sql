USE master;
GO
IF DB_ID (N'DB14_1') IS NOT NULL
ALTER DATABASE DB14_1 SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
DROP DATABASE DB14_1;
GO

CREATE DATABASE DB14_1
ON PRIMARY
(
  NAME = lab14_1dat,
  FILENAME = '/var/opt/mssql/data/lab14_1.mdf',
  SIZE = 10,
  MAXSIZE = UNLIMITED,
  FILEGROWTH = 5%
)
LOG ON
(
  NAME = lab14_1log,
  FILENAME = '/var/opt/mssql/data/lab14_1log.ldf',
  SIZE = 5MB,
  MAXSIZE = 25MB,
  FILEGROWTH = 5MB
)
GO
IF DB_ID (N'DB14_2') IS NOT NULL
ALTER DATABASE DB14_2 SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
DROP DATABASE DB14_2;
GO

CREATE DATABASE DB14_2
ON PRIMARY
(
  NAME = lab14_2dat,
  FILENAME = '/var/opt/mssql/data/lab14_2.mdf',
  SIZE = 10,
  MAXSIZE = UNLIMITED,
  FILEGROWTH = 5%
)
LOG ON
(
  NAME = lab14_2log,
  FILENAME = '/var/opt/mssql/data/lab14_2log.ldf',
  SIZE = 5MB,
  MAXSIZE = 25MB,
  FILEGROWTH = 5MB
)
GO

---

USE DB14_1;

DROP TABLE IF EXISTS USERS;

CREATE TABLE USERS(
    id INT PRIMARY KEY,
    email nvarchar(100) UNIQUE NOT NULL
        CHECK(LEN(email) > 6 AND email LIKE '%_@__%.__%'),
    phone_number char(11) NOT NULL CHECK(LEN(phone_number) = 11),
);

GO

USE DB14_2;

DROP TABLE IF EXISTS USERS;

CREATE TABLE USERS(
    id INT PRIMARY KEY,
    firstname nvarchar(100) NOT NULL CHECK(LEN(firstname) > 1),
    lastname nvarchar(100) NOT NULL CHECK(LEN(lastname) > 1),
    registration_date date NOT NULL DEFAULT(GETDATE())
    CONSTRAINT UQ_user UNIQUE(firstname, lastname, registration_date),
);

---

DROP VIEW IF EXISTS SectionUsers_View;
GO

CREATE VIEW SectionUsers_VIEW AS
    SELECT o.id, o.email, o.phone_number, t.firstname, t.lastname, t.registration_date
    FROM DB14_1.dbo.USERS as o, DB14_2.dbo.USERS as t
    WHERE o.id = t.id;
GO

DROP TRIGGER IF EXISTS ViewInsert;
DROP TRIGGER IF EXISTS ViewDelete;
DROP TRIGGER IF EXISTS ViewUpdate;
GO

CREATE TRIGGER ViewInsert ON SectionUsers_VIEW
INSTEAD OF INSERT
AS
BEGIN
    INSERT INTO DB14_1.dbo.USERS(id, email, phone_number)
    SELECT id, email, phone_number FROM inserted;

    INSERT INTO DB14_2.dbo.USERS(id, firstname, lastname, registration_date)
    SELECT id, firstname, lastname, registration_date FROM inserted;
END
GO

CREATE TRIGGER ViewUpdate ON SectionUsers_VIEW
INSTEAD OF UPDATE
AS
BEGIN
    IF UPDATE(id)
        BEGIN
            RAISERROR('ID can''t be updated', 16, 1);
            ROLLBACK TRANSACTION;
            RETURN;
        END
    ELSE
        BEGIN
            UPDATE DB14_1.dbo.USERS
            SET phone_number = u.phone_number, email = u.email
            FROM DB14_1.dbo.USERS as o INNER JOIN inserted AS u ON o.id = u.id;

            UPDATE DB14_2.dbo.USERS
            SET firstname = u.firstname, lastname = u.lastname, registration_date = u.registration_date
            FROM DB14_2.dbo.USERS as o INNER JOIN inserted AS u ON o.id = u.id;
        END
END
GO

CREATE TRIGGER ViewDelete ON SectionUsers_VIEW
INSTEAD OF DELETE
AS
BEGIN
    DELETE o
    FROM DB14_1.dbo.USERS as o INNER JOIN deleted AS u ON o.id = u.id;

    DELETE o
    FROM DB14_2.dbo.USERS as o INNER JOIN deleted AS u ON o.id = u.id;
END
GO

INSERT INTO SectionUsers_VIEW(id, email, phone_number, firstname, lastname, registration_date) VALUES
(1, 'test@test.su', '79123134123', 'Ivan', 'Gordeev', GETDATE()),
(2, 'ya@ya.com', '71234567890', 'First', 'Last', GETDATE());

SELECT * FROM SectionUsers_VIEW ORDER BY id;
SELECT * FROM DB14_1.dbo.USERS;
SELECT * FROM DB14_2.dbo.USERS;
GO

UPDATE SectionUsers_VIEW
SET email = 'ivan@gordeev.ru' WHERE id = 1;
SELECT * FROM DB14_1.dbo.USERS;
SELECT * FROM DB14_2.dbo.USERS;
GO

DELETE FROM SectionUsers_VIEW
WHERE LOWER(email) NOT LIKE '%.ru';

SELECT * FROM DB14_1.dbo.USERS;
SELECT * FROM DB14_2.dbo.USERS;
GO
