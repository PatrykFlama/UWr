----- create tables ------
-- local temporary table
CREATE TABLE #temp_local (id INT, name VARCHAR(50))
INSERT INTO #temp_local VALUES (1, 'John'), (2, 'Jane'), (3, 'Bob')
GO

-- global temporary table
CREATE TABLE ##temp_global (id INT, name VARCHAR(50))
INSERT INTO ##temp_global VALUES (1, 'Alice'), (2, 'Bob'), (3, 'Charlie')
GO

-- table variable
DECLARE @table_variable TABLE (id INT, name VARCHAR(50))
INSERT INTO @table_variable VALUES (1, 'David'), (2, 'Emily'), (3, 'Frank')
GO

------ check if table exists in current session ------
SELECT * FROM tempdb.INFORMATION_SCHEMA.tables WHERE table_name LIKE '%#temp_local%'
SELECT * FROM tempdb.INFORMATION_SCHEMA.tables WHERE table_name LIKE '%##temp_global%'
SELECT * FROM tempdb.INFORMATION_SCHEMA.tables WHERE table_name LIKE '%@table_variable%'

IF EXISTS (SELECT 1 FROM sys.dm_exec_describe_first_result_set(N'SELECT * FROM @table_variable', NULL, 1))
    PRINT 'The table variable still exists.'
ELSE
    PRINT 'The table variable has been dropped.'
GO


-- Open a new session and check if the local temporary table exists
-- Note: This must be executed in a separate query window or connection
SELECT * FROM tempdb.INFORMATION_SCHEMA.tables WHERE table_name LIKE '#temp_local%'
SELECT * FROM tempdb.INFORMATION_SCHEMA.tables WHERE table_name LIKE '##temp_global%'
SELECT * FROM tempdb.INFORMATION_SCHEMA.tables WHERE table_name LIKE '@table_variable%'

IF EXISTS (SELECT 1 FROM sys.dm_exec_describe_first_result_set(N'SELECT * FROM @table_variable', NULL, 1))
    PRINT 'The table variable still exists.'
ELSE
    PRINT 'The table variable has been dropped.'
GO

-- Wait for 10 seconds to simulate batch execution lifetime
WAITFOR DELAY '00:00:10'
GO

SELECT * FROM tempdb.INFORMATION_SCHEMA.tables WHERE table_name LIKE '#temp_local%'
SELECT * FROM tempdb.INFORMATION_SCHEMA.tables WHERE table_name LIKE '##temp_global%'
SELECT * FROM tempdb.INFORMATION_SCHEMA.tables WHERE table_name LIKE '@table_variable%'

IF EXISTS (SELECT 1 FROM sys.dm_exec_describe_first_result_set(N'SELECT * FROM @table_variable', NULL, 1))
    PRINT 'The table variable still exists.'
ELSE
    PRINT 'The table variable has been dropped.'
GO

----- drop tables -----
DROP TABLE #temp_local
DROP TABLE ##temp_global

------ check if table exists after dropping them ------
SELECT * FROM tempdb.INFORMATION_SCHEMA.tables WHERE table_name LIKE '#temp_local%'
SELECT * FROM tempdb.INFORMATION_SCHEMA.tables WHERE table_name LIKE '##temp_global%'
SELECT * FROM tempdb.INFORMATION_SCHEMA.tables WHERE table_name LIKE '@table_variable%'

IF EXISTS (SELECT 1 FROM sys.dm_exec_describe_first_result_set(N'SELECT * FROM @table_variable', NULL, 1))
    PRINT 'The table variable still exists.'
ELSE
    PRINT 'The table variable has been dropped.'
GO
