CREATE TABLE Test (
    ID INT IDENTITY(1000,10) PRIMARY KEY,
);
-- SELECT * FROM dbo.Test

-- @@IDENTITY - last identity value generated for any table in the current session, across all tables
-- IDENT_CURRENT(name) - last identity value generated for a specific table in any session for a specific table
