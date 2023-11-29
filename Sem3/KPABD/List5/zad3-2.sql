BEGIN TRANSACTION
insert liczby values(151)
COMMIT
go

SELECT *
FROM sys.dm_tran_locks
WHERE request_session_id = 64
