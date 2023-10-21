DROP PROCEDURE IF EXISTS GetBorrowedDaysByReaderID
DROP TYPE IF EXISTS dbo.czytelnik_id
GO

CREATE TYPE dbo.czytelnik_id AS TABLE
(
    Czytelnik_ID INT
);
GO

CREATE PROCEDURE GetBorrowedDaysByReaderID
    @reader_ids dbo.czytelnik_id READONLY
AS
BEGIN
    SELECT r.Czytelnik_ID, SUM(Wypozyczenie.Liczba_Dni) Suma_Dni_Wypozyczenia
    FROM @reader_ids r RIGHT JOIN Wypozyczenie ON r.Czytelnik_ID = Wypozyczenie.Czytelnik_ID
    GROUP BY r.Czytelnik_ID
END
GO


-- test
DECLARE @CzytelnikIds dbo.czytelnik_id
INSERT INTO @CzytelnikIds (Czytelnik_ID)
    SELECT Czytelnik_ID FROM Czytelnik

EXEC GetBorrowedDaysByReaderID @reader_ids = @CzytelnikIds
GO
