DROP PROCEDURE IF EXISTS AddReader
GO

CREATE PROCEDURE AddReader
    @pesel VARCHAR(11),
    @lastName VARCHAR(50),
    @city VARCHAR(50),
    @birthDate DATE
AS
BEGIN
    -- Validate PESEL format
    IF @pesel NOT LIKE '' + REPLICATE('[0-9]', 11)
    BEGIN
        THROW 50001, 'Invalid PESEL format', 1;
        RETURN;
    END

    -- Validate last name format
    IF @lastName NOT LIKE '[A-Z][A-Za-z]%' COLLATE Latin1_General_CS_AS
    BEGIN
        THROW 50002, 'Invalid last name format', 1;
        RETURN;
    END

    -- Validate birth date format
    IF @birthDate NOT LIKE '[0-9][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]'
    BEGIN
        THROW 50003, 'Invalid date format', 1;
        RETURN;
    END
    IF @birthDate > GETDATE()
    BEGIN
        THROW 50003, 'Birth date in the future', 1;
        RETURN;
    END

    -- Insert new reader
    INSERT INTO Czytelnik (PESEL, Nazwisko, Miasto, Data_Urodzenia)
    VALUES (@pesel, @lastName, @city, @birthDate);
END
GO

-- test
EXEC AddReader '12345678901', 'Kowalski', 'Warszawa', '1990-01-01';
GO
EXEC AddReader '12345678901', 'Nowak', 'Random', '2077-01-01';
GO
EXEC AddReader '12345678901', 'abc', 'Hell', '1990-01-01';
GO
EXEC AddReader '12345678901', '2abc', 'Hell', '1990-01-01';
GO
EXEC AddReader '12345678901', '2abc', 'Aaa', 'date';
GO

SELECT * FROM Czytelnik
GO

DELETE FROM Czytelnik
WHERE PESEL = '12345678901'
GO