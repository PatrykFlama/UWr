SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED; 
-- SET TRANSACTION ISOLATION LEVEL READ COMMITTED;
-- SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;
-- SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;

-- S1:
drop table if exists liczby1;
drop table if exists liczby2;
create table liczby1 ( liczba int )
create table liczby2 ( liczba int )
go

-- S1:
begin tran
insert liczby1 values ( 1 )

-- S2:
begin tran
insert liczby2 values ( 1 )

-- S1:
-- update liczby2 set liczba=10

-- S2:
update liczby1 set liczba=10

-- and here we have a deadlock
/*
deadlock happens because first transaction is waiting for the second one to finish
(since it wants to acces second table)
and the second one is waiting for the first one to finish
(since it wants to acces first table)
*/
