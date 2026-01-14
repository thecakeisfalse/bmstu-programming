USE LLM_DB10;
GO

/*BEGIN TRANSACTION
    UPDATE USERS SET email = 'dm_tran@locks.com' WHERE id = 1;
    WAITFOR DELAY '00:00:06';
    SELECT * FROM USERS;
    SELECT * FROM sys.dm_tran_locks;
ROLLBACK TRANSACTION;
GO*/

/*BEGIN TRANSACTION
    UPDATE USERS SET phone_number = '77777777777' WHERE email LIKE '%zmail%';
    SELECT * FROM USERS;
    SELECT * FROM sys.dm_tran_locks;
COMMIT TRANSACTION;
GO*/

/*BEGIN TRANSACTION
    INSERT INTO USERS(email, phone_number) VALUES
    ('doesnt@exist.com', '79991112233');
    SELECT * FROM USERS;
    SELECT * FROM sys.dm_tran_locks;
COMMIT TRANSACTION;
GO*/