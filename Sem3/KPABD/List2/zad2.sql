-- Drop tables
DROP TABLE IF EXISTS firstnames;
DROP TABLE IF EXISTS lastnames;
DROP TABLE IF EXISTS fldata;
GO

-- Create tables
CREATE TABLE firstnames (
    id INT PRIMARY KEY,
    firstname VARCHAR(50)
);

CREATE TABLE lastnames (
    id INT PRIMARY KEY,
    lastname VARCHAR(50)
);

CREATE TABLE fldata (
    firstname VARCHAR(50),
    lastname VARCHAR(50),
    PRIMARY KEY (firstname, lastname)
);

-- Generate test data
INSERT INTO firstnames VALUES
    (1, 'John'),
    (2, 'Jane'),
    (3, 'Bob'),
    (4, 'Alice'),
    (5, 'Mike');

INSERT INTO lastnames VALUES
    (1, 'Doe'),
    (2, 'Smith'),
    (3, 'Johnson'),
    (4, 'Brown'),
    (5, 'Davis');
GO


-- Create procedure
DROP PROCEDURE IF EXISTS dbo.regenerateNames
GO

CREATE PROCEDURE dbo.regenerateNames @N INT AS
BEGIN
    DECLARE @SFN INT, @SLN INT;
    SET @SFN = (SELECT COUNT(*) FROM firstnames);
    SET @SLN = (SELECT COUNT(*) FROM lastnames);
    IF @N > @SFN * @SLN
    BEGIN
        ;THROW 50000, 'N is too big', 1;
        -- RAISERROR('N is too big', 16, 1);
        -- RETURN;
    END

    DELETE FROM fldata

    INSERT INTO fldata
        SELECT TOP (@N) firstname, lastname
        FROM firstnames, lastnames
        ORDER BY NEWID()
END
GO


-- Test procedure
EXEC dbo.regenerateNames @N = 10;
SELECT * FROM fldata;
EXEC dbo.regenerateNames @N = 4;
SELECT * FROM fldata;
EXEC dbo.regenerateNames @N = 10000;
SELECT * FROM fldata;


