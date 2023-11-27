BEGIN TRANSACTION
update liczby set liczba=6
COMMIT

go
SELECT *
FROM sys.dm_tran_locks
WHERE request_session_id = 75
