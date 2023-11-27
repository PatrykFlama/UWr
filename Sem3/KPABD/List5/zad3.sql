-- rodzaje blokad
drop table if exists liczby;
go
create table liczby ( liczba int );
go
insert liczby values ( 10 );
go

SELECT @@SPID;
GO

-- 1 --
-- set transaction isolation level repeatable read;
-- begin transaction

-- WAITFOR DELAY '00:00:5'
-- -- run zad3-1.sql

-- select * from liczby

-- commit

-- 2 --
set transaction isolation level serializable;
GO

insert liczby values ( 10 );
-- the second insert will wait for the this transaction to finish

begin transaction

WAITFOR DELAY '00:00:5'

select * from liczby

commit

-- 3 --
-- ALTER DATABASE AdvWorksLT SET ALLOW_SNAPSHOT_ISOLATION ON
-- GO

-- set transaction isolation level snapshot;
-- begin transaction

-- select * from liczby

-- WAITFOR DELAY '00:00:5'

-- update liczby set liczba=7

-- select * from liczby

-- commit
