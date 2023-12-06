-- BEGIN: Create Clustered Index
CREATE CLUSTERED INDEX IX_Ksiazka ON Ksiazka (column1, column2);
-- END: Create Clustered Index

-- BEGIN: Create Non-Clustered Index
CREATE NONCLUSTERED INDEX IX_Egzemplarz ON Egzemplarz (column3, column4);
-- END: Create Non-Clustered Index

-- BEGIN: Create Covering Index
CREATE NONCLUSTERED INDEX IX_Covering ON Ksiazka (column1, column2)
INCLUDE (column3, column4);
-- END: Create Covering Index

