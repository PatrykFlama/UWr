CREATE FUNCTION dbo.funGetAfterDeadline(@MAX_DAYS INT)
RETURNS TABLE
    RETURN SELECT PESEL, COUNT(*) Liczba_ksiazek
    FROM ((SELECT DISTINCT Czytelnik_ID FROM Wypozyczenie WHERE Liczba_Dni > @MAX_DAYS) u 
        INNER JOIN Czytelnik ON Czytelnik.Czytelnik_ID = u.Czytelnik_ID) 
        LEFT JOIN Wypozyczenie ON Wypozyczenie.Czytelnik_ID = Czytelnik.Czytelnik_ID
    WHERE u.Czytelnik_ID IS NOT NULL
    GROUP BY PESEL
GO

SELECT * FROM dbo.funGetAfterDeadline(13)
