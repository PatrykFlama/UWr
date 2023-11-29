BEGIN TRANSACTION
update liczby set liczba=4
COMMIT 

go
SELECT *
FROM sys.dm_tran_locks
WHERE request_session_id = 64
