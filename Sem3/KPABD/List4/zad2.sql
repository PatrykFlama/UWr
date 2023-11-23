DROP TABLE IF EXISTS SalesLT.ProductPriceHistory;
GO

CREATE TABLE SalesLT.ProductPriceHistory (
    ProductID INT,
    StandardCost DECIMAL(10,2),
    ListPrice DECIMAL(10,2),
    StartDate DATETIME,
    EndDate DATETIME,
);
GO

---------------------------
DROP TRIGGER IF EXISTS SalesLT.update_ProductPriceHistory
DROP TRIGGER IF EXISTS SalesLT.delete_ProductPriceHistory
GO

CREATE TRIGGER update_ProductPriceHistory
ON SalesLT.Product
AFTER UPDATE
AS
BEGIN
    IF UPDATE(StandardCost) OR UPDATE(ListPrice)
    BEGIN
        UPDATE SalesLT.ProductPriceHistory
        SET EndDate = GETDATE()
        WHERE ProductID IN (SELECT ProductID FROM inserted) AND EndDate IS NULL;

        INSERT INTO SalesLT.ProductPriceHistory (ProductID, StandardCost, ListPrice, StartDate, EndDate)
        SELECT ProductID, StandardCost, ListPrice, GETDATE(), NULL
        FROM inserted;
    END
END
GO

CREATE TRIGGER delete_ProductPriceHistory
ON SalesLT.Product
FOR DELETE
AS
BEGIN
    UPDATE SalesLT.ProductPriceHistory
    SET EndDate = GETDATE()
    WHERE ProductID IN (SELECT ProductID FROM deleted) AND EndDate IS NULL;
END
GO

-- TEST:
INSERT INTO SalesLT.Product (Name, ProductNumber, Color, StandardCost, ListPrice, SellStartDate)
    VALUES ('Test', '123456789', 'Red', 10, 20, GETDATE());

SELECT * FROM SalesLT.ProductPriceHistory;

WAITFOR DELAY '00:00:01';
UPDATE SalesLT.Product
SET StandardCost = 15
WHERE Name = 'Test';

WAITFOR DELAY '00:00:01';
UPDATE SalesLT.Product
SET ListPrice = 25
WHERE Name = 'Test';

WAITFOR DELAY '00:00:01';
UPDATE SalesLT.Product
SET StandardCost = 20, ListPrice = 30
WHERE Name = 'Test';

SELECT * FROM SalesLT.ProductPriceHistory;


DELETE FROM SalesLT.Product
WHERE Name = 'Test';

