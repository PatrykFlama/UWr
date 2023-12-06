DROP PROCEDURE IF EXISTS List6Ex2P1
DROP PROCEDURE IF EXISTS List6Ex2P2
DROP PROCEDURE IF EXISTS List6Ex2P3
GO

CREATE PROCEDURE List6Ex2P1
AS
BEGIN
    SELECT DISTINCT c.PESEL, c.Nazwisko
    FROM Egzemplarz e
    JOIN Ksiazka k ON e.Ksiazka_ID=k.Ksiazka_ID
    JOIN Wypozyczenie w ON e.Egzemplarz_ID=w.Egzemplarz_ID
    JOIN Czytelnik c ON c.Czytelnik_ID = w.Czytelnik_ID;
END
GO

CREATE PROCEDURE List6Ex2P2
AS
BEGIN
    SELECT c.PESEL, c.Nazwisko
    FROM Czytelnik c WHERE c.Czytelnik_ID IN
    (SELECT w.Czytelnik_ID FROM Wypozyczenie w
    JOIN Egzemplarz e ON e.Egzemplarz_ID=w.Egzemplarz_ID
    JOIN Ksiazka k ON e.Ksiazka_ID=k.Ksiazka_ID)
END
GO

CREATE PROCEDURE List6Ex2P3
AS
BEGIN
    SELECT DISTINCT c.PESEL, c.Nazwisko
    FROM Czytelnik c
    WHERE c.Czytelnik_ID IN (
        SELECT DISTINCT w.Czytelnik_ID
        FROM Wypozyczenie w
        WHERE w.Egzemplarz_ID IN (
            SELECT e.Egzemplarz_ID
            FROM Egzemplarz e
            JOIN Ksiazka k ON e.Ksiazka_ID=k.Ksiazka_ID
        )
    );
END
GO


SET SHOWPLAN_ALL ON;
GO

EXEC List6Ex2P1
EXEC List6Ex2P2
EXEC List6Ex2P3
GO

SET SHOWPLAN_ALL OFF;
GO


/* #region GENERATING DATA */
DELETE FROM Wypozyczenie
DELETE FROM Egzemplarz
DELETE FROM Ksiazka
DELETE FROM Czytelnik
GO

SET IDENTITY_INSERT Ksiazka ON
INSERT INTO Ksiazka (Ksiazka_ID,ISBN,Tytul,Autor,Rok_Wydania,Cena) VALUES
(1,'83-246-0279-8','Microsoft Access. Podr�cznik administratora','Helen Feddema',2006,69),
(2,'83-246-0653-X','SQL Server 2005. Programowanie. Od podstaw','Robert Vieira',2007,97),
(3,'978-83-246-0549-1','SQL Server 2005. Wyci�nij wszystko','Eric L. Brown',2007,57),
(4,'978-83-246-1258-1','PHP, MySQL i MVC. Tworzenie witryn WWW opartych na bazie danych','W�odzimierz Gajda',2010,79),
(5,'978-83-246-2060-9','Access 2007 PL. Seria praktyk','Andrew Unsworth',2009,39),
(6,'978-83-246-2188-0','Czysty kod. Podr�cznik dobrego programisty','Robert C. Martin',2010,67);
SET IDENTITY_INSERT Ksiazka OFF
GO

SET IDENTITY_INSERT Egzemplarz ON
INSERT INTO Egzemplarz (Egzemplarz_ID,Ksiazka_ID,Sygnatura) VALUES
(1,5,'S0001'),
(2,5,'S0002'),
(3,1,'S0003'),
(4,1,'S0004'),
(5,1,'S0005'),
-- (6,2,'S0006'),
(7,3,'S0007'),
(8,3,'S0008'),
(9,3,'S0009'),
(10,3,'S0010'),
(11,6,'S0011'),
(12,6,'S0012'),
(13,4,'S0013'),
(14,4,'S0014'),
(15,4,'S0015');
SET IDENTITY_INSERT Egzemplarz OFF
GO

SET IDENTITY_INSERT Czytelnik ON
INSERT INTO Czytelnik (CZYTELNIK_ID,PESEL,NAZWISKO,MIASTO,DATA_URODZENIA) VALUES
(1,'55101011111','Kowalski','Wroc�aw','1955-10-10'),
(2,'60101033333','Maliniak','Wroc�aw','1960-10-10'),
(3,'65120122222','Nowak','Warszawa','1965-12-01');
SET IDENTITY_INSERT Czytelnik OFF
GO

SET IDENTITY_INSERT Wypozyczenie ON
INSERT INTO Wypozyczenie (Wypozyczenie_ID,Czytelnik_ID,Egzemplarz_ID,Data,Liczba_Dni) VALUES
(1,1,3,'2020-02-01',12),
(2,1,4,'2020-01-05',20),
(3,1,15,'2020-01-21',45),
(4,2,8,'2020-01-13',7),
(5,3,4,'2020-02-01',14),
(6,3,12,'2020-02-02',10),
(7,3,12,'2020-02-12',3),
(8,3,12,'2020-02-16',4),
(9,1,12,'2020-02-20',2),
(10,2,12,'2020-02-22',5),
(11,2,12,'2020-02-28',12),
(12,1,12,'2020-03-10',8),
(13,3,12,'2020-03-15',4);
SET IDENTITY_INSERT Wypozyczenie OFF
GO

ALTER TABLE Wypozyczenie NOCHECK CONSTRAINT ALL;
ALTER TABLE Egzemplarz NOCHECK CONSTRAINT ALL;
ALTER TABLE Ksiazka NOCHECK CONSTRAINT ALL;
ALTER TABLE Czytelnik NOCHECK CONSTRAINT ALL;
GO

DECLARE @counter INT = 1;
WHILE @counter <= 100
BEGIN
    INSERT INTO Czytelnik (PESEL, NAZWISKO, MIASTO, DATA_URODZENIA)
    VALUES (@counter, 'John Doe', 'temp', '2000-01-01');

    INSERT INTO Ksiazka (ISBN, Tytul, Autor, Rok_Wydania, Cena)
    VALUES (@counter, 'temp', 'John Smith', 2022, 9.99);

    INSERT INTO Egzemplarz (Ksiazka_ID, Sygnatura)
    VALUES (1, @counter);

    INSERT INTO Wypozyczenie (Czytelnik_ID, Egzemplarz_ID, Data, Liczba_Dni)
    VALUES (1, 1, '2022-01-01', 7);
    
    SET @counter = @counter + 1;
END
GO

ALTER TABLE Czytelnik CHECK CONSTRAINT ALL;
ALTER TABLE Ksiazka CHECK CONSTRAINT ALL;
ALTER TABLE Egzemplarz CHECK CONSTRAINT ALL;
ALTER TABLE Wypozyczenie CHECK CONSTRAINT ALL;
GO
/* #endregion */


DECLARE @StartTime1 DATETIME, @EndTime1 DATETIME;
DECLARE @StartTime2 DATETIME, @EndTime2 DATETIME;
DECLARE @StartTime3 DATETIME, @EndTime3 DATETIME;

SET @StartTime1 = GETDATE();
EXEC List6Ex2P1;
SET @EndTime1 = GETDATE();

SET @StartTime2 = GETDATE();
EXEC List6Ex2P2;
SET @EndTime2 = GETDATE();

SET @StartTime3 = GETDATE();
EXEC List6Ex2P3;
SET @EndTime3 = GETDATE();

DECLARE @Runtime1 INT, @Runtime2 INT, @Runtime3 INT;
SET @Runtime1 = DATEDIFF(ms, @StartTime1, @EndTime1);
SET @Runtime2 = DATEDIFF(ms, @StartTime2, @EndTime2);
SET @Runtime3 = DATEDIFF(ms, @StartTime3, @EndTime3);

PRINT 'Runtime of List6Ex2P1: ' + CAST(@Runtime1 AS VARCHAR) + ' ms';
PRINT 'Runtime of List6Ex2P2: ' + CAST(@Runtime2 AS VARCHAR) + ' ms';
PRINT 'Runtime of List6Ex2P3: ' + CAST(@Runtime3 AS VARCHAR) + ' ms';
GO