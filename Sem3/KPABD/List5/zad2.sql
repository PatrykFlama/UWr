------- set isolation level -------
SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;
-- SET TRANSACTION ISOLATION LEVEL READ COMMITTED;
-- SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;
-- SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;

-- IZOLATION LEVEL		DIRTY READ		NONREAPEATABLE READ		PHANTOM
-- READ UNCOMMITTED		YES				YES						YES
-- READ COMMITTED		NO 				YES						YES
-- REPEATABLE READ		NO 				NO 						YES
-- SERIALIZABLE			NO 				NO 						NO 

-------- Dirty read --------
DROP TABLE IF EXISTS Test
GO

CREATE TABLE Test (ID INT PRIMARY KEY, Product VARCHAR(50), Pcs INT)
INSERT INTO Test VALUES(1, 'P1', 6)
INSERT INTO Test VALUES(2, 'P1', 7)
INSERT INTO Test VALUES(3, 'P3', 8)
INSERT INTO Test VALUES(4, 'P4', 9)
INSERT INTO Test VALUES(5, 'P5', 10)
GO

-- Transaction 1 --
BEGIN TRAN
UPDATE Test SET Pcs = 0 WHERE ID = 1
UPDATE Test SET Pcs = 0  WHERE ID = 4

WAITFOR DELAY '00:00:05'

SELECT * FROM Test
ROLLBACK TRANSACTION
SELECT * FROM Test

-- Transaction 2 --
-- SELECT * FROM Test


-------- Nonrepeatable read --------
-- DROP TABLE IF EXISTS People 
-- GO

-- CREATE TABLE People (Name VARCHAR(20), Surname VARCHAR(20), Age INT)
-- INSERT INTO People VALUES('Name1', 'Surname1', 20)
-- INSERT INTO People VALUES('Name2', 'Surname2', 21)
-- INSERT INTO People VALUES('Name3', 'Surname3', 22)
-- INSERT INTO People VALUES('Name4', 'Surname4', 23)
-- INSERT INTO People VALUES('Name5', 'Surname5', 24)
-- GO

-- -- Transaction 1 --
-- BEGIN TRAN
-- SELECT * FROM People
-- WAITFOR DELAY '00:00:05'
-- SELECT * FROM People
-- ROLLBACK
-- GO

-- -- Transaction 2 --
-- -- BEGIN TRAN
-- -- UPDATE People SET Age = 30 WHERE Name = 'Name1' OR Name = 'Name2'
-- -- COMMIT
-- -- GO


-------- Phantom read --------
-- DROP TABLE IF EXISTS Cars
-- GO

-- CREATE TABLE Cars (Model VARCHAR(20), Brand VARCHAR(20))
-- INSERT INTO Cars VALUES('Model1', 'Brand1')
-- INSERT INTO Cars VALUES('Model1', 'Brand2')
-- INSERT INTO Cars VALUES('Model1', 'Brand3')
-- INSERT INTO Cars VALUES('Model2', 'Brand4')
-- INSERT INTO Cars VALUES('Model2', 'Brand5')
-- GO

-- -- Transaction 1 --
-- BEGIN TRAN
-- SELECT * FROM Cars
-- WAITFOR DELAY '00:00:05'
-- SELECT * FROM Cars
-- ROLLBACK

-- -- Transaction 2 --
-- -- BEGIN TRAN
-- -- DELETE FROM Cars WHERE Model = 'Model1'
-- -- COMMIT
-- -- GO


