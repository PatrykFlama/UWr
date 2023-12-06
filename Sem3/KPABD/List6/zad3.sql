SELECT
    TableName = t.name, 
    ClusteredIndexName = i.name,
    ColumnName = c.Name
FROM
    sys.tables t
INNER JOIN 
    sys.indexes i ON t.object_id = i.object_id
INNER JOIN 
    sys.index_columns ic ON i.index_id = ic.index_id AND i.object_id = ic.object_id
INNER JOIN 
    sys.columns c ON ic.column_id = c.column_id AND ic.object_id = c.object_id
WHERE
    i.index_id = 1  -- clustered index
    AND EXISTS (SELECT * 
                FROM sys.columns c2 
                WHERE ic.object_id = c2.object_id AND c2.is_identity = 1)
GO


DROP INDEX Ksiazka_PK ON Ksiazka;
CREATE CLUSTERED INDEX ClusteredIX_Ksiazka ON Ksiazka (Ksiazka_ID, Rok_Wydania DESC, Tytul ASC);

SELECT TOP 10 Rok_Wydania, Tytul FROM Ksiazka;


DROP INDEX ClusteredIX_Ksiazka ON Ksiazka;
GO

CREATE NONCLUSTERED INDEX NonClusteredIX_Egzemplarz ON Egzemplarz (Ksiazka_ID, Sygnatura);

SELECT TOP 10 Sygnatura, Ksiazka_ID FROM Egzemplarz;

DROP INDEX NonClusteredIX_Egzemplarz ON Egzemplarz;
GO

-- Index optimized for join
CREATE NONCLUSTERED INDEX IX_Egzemplarz_Ksiazka_ID ON Egzemplarz (Ksiazka_ID);
CREATE NONCLUSTERED INDEX IX_Ksiazka_ID ON Ksiazka (Ksiazka_ID);

SET SHOWPLAN_ALL ON;
SELECT TOP 10 * FROM Ksiazka JOIN Egzemplarz ON Ksiazka.Ksiazka_ID = Egzemplarz.Ksiazka_ID;
SET SHOWPLAN_ALL OFF;

DROP INDEX IX_Egzemplarz_Ksiazka_ID ON Egzemplarz;
DROP INDEX IX_Ksiazka_ID ON Ksiazka;
GO
