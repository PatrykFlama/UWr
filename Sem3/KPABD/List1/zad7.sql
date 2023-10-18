-- creating new table
-- CREATE TABLE Test (
--     ID INT IDENTITY(1000,10) PRIMARY KEY,
-- );

-- modifying/adding table columns
-- ALTER TABLE Test
-- ADD Numbers INT NOT NULL DEFAULT 0;

-- inserting data
-- INSERT INTO dbo.Test DEFAULT VALUES
-- INSERT INTO dbo.Test (Numbers) VALUES (1),(2),(3),(4),(5),(6),(7),(8),(9),(10)

-- @@IDENTITY - last identity value generated for any table in the current session, across all tables
-- IDENT_CURRENT(name) - last identity value generated for a specific table in any session for a specific table

SELECT * FROM dbo.Test
SELECT @@IDENTITY
SELECT IDENT_CURRENT('dbo.Test')
SELECT IDENT_CURRENT('SalesLT.Address')