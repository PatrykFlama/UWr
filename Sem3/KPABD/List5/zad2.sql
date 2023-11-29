------- set isolation level -------
SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;
-- SET TRANSACTION ISOLATION LEVEL READ COMMITTED;
-- SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;
-- SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;

-- POZIOM IZOLACJI		DIRTY READ		NONREAPEATABLE READ		PHANTOM
-- READ UNCOMMITTED		YES				YES						YES
-- READ COMMITTED		NO 				YES						YES
-- REPEATABLE READ		NO 				NO 						YES
-- SERIALIZABLE			NO 				NO 						NO 

-------- Dirty read --------
DROP TABLE IF EXISTS Test
GO

CREATE TABLE Test (ID INT PRIMARY KEY, Product VARCHAR(50), Pcs INT)
INSERT INTO Test VALUES(1, 'P1', 13)
INSERT INTO Test VALUES(2, 'P1', 3)
INSERT INTO Test VALUES(3, 'P3', 4)
INSERT INTO Test VALUES(4, 'P4', 6)
INSERT INTO Test VALUES(5, 'P5', 1)
GO

-- Transaction 1 --
BEGIN TRAN
UPDATE Test SET Pcs = 12 WHERE ID = 1
UPDATE Test SET Pcs = 3  WHERE ID = 4
-- czekamy na przyjêcie pieniêdzy od klienta, st¹d delay
WAITFOR DELAY '00:00:05'
SELECT * FROM Test -- dane zmieniaja sie, ale rollback je cofa
ROLLBACK TRANSACTION
SELECT * FROM Test

-- Transaction 2 --
SELECT * FROM Test


-------- Nonrepeatable read --------
DROP TABLE IF EXISTS People 
GO

CREATE TABLE People (Imie VARCHAR(20), Nazwisko VARCHAR(20), Wiek INT)
INSERT INTO People VALUES('Tomasz', 'Kowalski', 20)
INSERT INTO People VALUES('Oskar',  'Nowak',    21)
INSERT INTO People VALUES('Jan',    'Kowalski', 22)
INSERT INTO People VALUES('Jan',    'Nowak',    23)
INSERT INTO People VALUES('Tomasz', 'Nowak',    24)
GO

-- Transaction 1 --
BEGIN TRAN
SELECT * FROM People
WAITFOR DELAY '00:00:05'
SELECT * FROM People
ROLLBACK
GO

-- Transaction 2 --
BEGIN TRAN
UPDATE People SET Wiek = 50 WHERE Imie = 'Tomasz' OR Imie = 'Oskar'
COMMIT
GO


-------- Phantom read --------
DROP TABLE IF EXISTS Cars
GO

CREATE TABLE Cars (Model VARCHAR(20), Marka VARCHAR(20))
INSERT INTO Cars VALUES('Opel',  'Astra')
INSERT INTO Cars VALUES('Opel',  'Meriva')
INSERT INTO Cars VALUES('Skoda', 'Fabia')
INSERT INTO Cars VALUES('Skoda', 'Superb')
INSERT INTO Cars VALUES('Skoda', 'Felicia')
GO

-- Transaction 1 --
BEGIN TRAN
SELECT * FROM Cars
WAITFOR DELAY '00:00:05'
SELECT * FROM Cars
ROLLBACK

-- Transaction 2 --
BEGIN TRAN
DELETE FROM Cars WHERE Model = 'Opel'
COMMIT
GO

