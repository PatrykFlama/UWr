CREATE PROCEDURE AddReader
    @pesel VARCHAR(11),
    @lastName VARCHAR(50),
    @city VARCHAR(50),
    @birthDate DATE
AS
BEGIN
    -- Validate PESEL format
    IF @pesel NOT LIKE '[0-9][0-9][0-1][0-9][0-3][0-9][0-9][0-9][0-9][0-9][0-9]'
    BEGIN
        THROW 50001, 'Invalid PESEL format', 1;
        RETURN;
    END

    -- Validate last name format
    IF @lastName NOT LIKE '[A-Z][a-z]%' COLLATE Latin1_General_CS_AS
    BEGIN
        THROW 50002, 'Invalid last name format', 1;
        RETURN;
    END

    -- Validate birth date format
    IF @birthDate > GETDATE()
    BEGIN
        THROW 50003, 'Invalid birth date', 1;
        RETURN;
    END

    -- Insert new reader
    INSERT INTO Czytelnik (PESEL, Nazwisko, Miasto, Data_Urodzenia)
    VALUES (@pesel, @lastName, @city, @birthDate);
END