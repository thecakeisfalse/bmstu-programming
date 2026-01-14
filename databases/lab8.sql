USE master;
GO
IF DB_ID (N'LLM_DB8') IS NOT NULL
ALTER DATABASE LLM_DB8 SET SINGLE_USER WITH ROLLBACK IMMEDIATE;
DROP DATABASE LLM_DB8;
GO

CREATE DATABASE LLM_DB8
ON PRIMARY
(
  NAME = lab8dat,
  FILENAME = '/var/opt/mssql/data/lab8.mdf',
  SIZE = 10,
  MAXSIZE = UNLIMITED,
  FILEGROWTH = 5%
)
LOG ON
(
  NAME = lab8log,
  FILENAME = '/var/opt/mssql/data/lab8log.ldf',
  SIZE = 5MB,
  MAXSIZE = 25MB,
  FILEGROWTH = 5MB
)
GO

--

USE LLM_DB8;
GO

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
('email@mail.com', '79003241234', 'Ivan', 'Surname'),
('user2@mail.su', '73241234123', 'Fernando', 'Allonso');
GO

SELECT * FROM USERS;

---

DROP PROCEDURE IF EXISTS dbo.users_information_cursor;
GO

CREATE PROCEDURE dbo.users_information_cursor
  @cursor CURSOR VARYING OUTPUT
AS
  SET @cursor = CURSOR
  FORWARD_ONLY STATIC FOR
  SELECT name, surname, email
  FROM USERS WHERE name = 'Ivan';
  OPEN @cursor;
GO


DECLARE @users_cursor CURSOR;
DECLARE @name nvarchar(100), @surname nvarchar(100), @email nvarchar(100);

EXECUTE dbo.users_information_cursor @cursor = @users_cursor OUTPUT;

FETCH NEXT FROM @users_cursor INTO @name, @surname, @email;
WHILE @@FETCH_STATUS = 0
BEGIN
    SELECT @name as name, @surname as surname, @email as email;
    FETCH NEXT FROM @users_cursor INTO @name, @surname, @email;
END
CLOSE @users_cursor;
DEALLOCATE @users_cursor;

GO

---

DROP PROCEDURE IF EXISTS dbo.users_information_cursor2;
GO

DROP FUNCTION IF EXISTS dbo.users_full_name;
GO

CREATE FUNCTION dbo.users_full_name
  (@name nvarchar(100), @surname nvarchar(100))
RETURNS NVARCHAR(201) AS
BEGIN
  DECLARE @result nvarchar(201);
  SET @result = @name + ' ' + @surname;
  RETURN @result;
END
GO

CREATE PROCEDURE dbo.users_information_cursor2
  @cursor CURSOR VARYING OUTPUT
AS
  SET @cursor = CURSOR
  FORWARD_ONLY STATIC FOR
  SELECT dbo.users_full_name(name, surname) AS fullname, email
  FROM USERS WHERE name = 'Ivan';
  OPEN @cursor;
GO

DECLARE @users_cursor CURSOR, @fullname nvarchar(201), @email nvarchar(100);
EXECUTE dbo.users_information_cursor2 @cursor = @users_cursor OUTPUT;

FETCH NEXT FROM @users_cursor INTO @fullname, @email;
WHILE @@FETCH_STATUS = 0
BEGIN
    SELECT @fullname as fullname, @email as email;
    FETCH NEXT FROM @users_cursor INTO @fullname, @email;
END
CLOSE @users_cursor;
DEALLOCATE @users_cursor;

GO

---

DROP PROCEDURE IF EXISTS dbo.count_users_with_mail;
GO

DROP FUNCTION IF EXISTS dbo.is_email_mail;
GO

CREATE FUNCTION dbo.is_email_mail (@email nvarchar(100))
RETURNS BIT AS
BEGIN
    IF @email LIKE '%@mail.%'
      RETURN 1
    RETURN 0
END
GO

CREATE PROCEDURE dbo.count_users_with_mail
AS
  DECLARE @current_cursor CURSOR;
  EXECUTE dbo.users_information_cursor @cursor = @current_cursor OUTPUT;

  DECLARE @name nvarchar(100), @surname nvarchar(100), @email nvarchar(100);

  FETCH NEXT FROM @current_cursor INTO @name, @surname, @email;
  WHILE @@FETCH_STATUS = 0
  BEGIN
    IF dbo.is_email_mail(@email) = 1
        PRINT @name + ' ' + @surname + ' ' + @email
    FETCH NEXT FROM @current_cursor INTO @name, @surname, @email;
  END
  CLOSE @current_cursor;
  DEALLOCATE @current_cursor;
GO

EXEC dbo.count_users_with_mail;
GO

---

DROP PROCEDURE IF EXISTS dbo.users_information_cursor;
GO

DROP FUNCTION IF EXISTS dbo.is_users_name_ivan;
GO

CREATE FUNCTION dbo.is_users_name_ivan ()
RETURNS TABLE AS
RETURN (
  SELECT name, surname, email
  FROM USERS WHERE name = 'Ivan'
)
GO

/*CREATE FUNCTION dbo.is_users_name_ivan ()
RETURNS @ResultTable TABLE
(
    name nvarchar(100),
    surname nvarchar(100),
    email nvarchar(100)
)
AS
BEGIN
    INSERT INTO @ResultTable
    SELECT name, surname, email
    FROM USERS WHERE name = 'Ivan';
    RETURN;
END
GO*/

CREATE PROCEDURE dbo.users_information_cursor
  @cursor CURSOR VARYING OUTPUT
AS
  SET @cursor = CURSOR
  FORWARD_ONLY STATIC FOR
  SELECT * FROM dbo.is_users_name_ivan()
  OPEN @cursor;
GO


DECLARE @users_cursor CURSOR;
DECLARE @name nvarchar(100), @surname nvarchar(100), @email nvarchar(100);

EXECUTE dbo.users_information_cursor @cursor = @users_cursor OUTPUT;

FETCH NEXT FROM @users_cursor INTO @name, @surname, @email;
WHILE @@FETCH_STATUS = 0
BEGIN
    SELECT @name as name, @surname as surname, @email as email;
    FETCH NEXT FROM @users_cursor INTO @name, @surname, @email;
END
CLOSE @users_cursor;
DEALLOCATE @users_cursor;

GO
