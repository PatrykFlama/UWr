DROP PROCEDURE IF EXISTS UpdateProductDiscontinuedDate
GO

CREATE PROCEDURE UpdateProductDiscontinuedDate
    @ProductIDs NVARCHAR(max),
    @DiscontinuedDate DATE
AS
BEGIN
    DECLARE @Message NVARCHAR(max)

    DECLARE @ProductID NVARCHAR(50)
    DECLARE ProductCursor CURSOR FOR
        SELECT value FROM STRING_SPLIT(@ProductIDs, ',')

    OPEN ProductCursor
    FETCH NEXT FROM ProductCursor INTO @ProductID

    WHILE @@FETCH_STATUS = 0
    BEGIN
        IF EXISTS (SELECT * FROM SalesLT.Product 
                   WHERE ProductID = @ProductID AND DiscontinuedDate IS NULL)
        BEGIN
            UPDATE SalesLT.Product 
            SET DiscontinuedDate = @DiscontinuedDate 
            WHERE ProductID = @ProductID
        END
        ELSE
        BEGIN
            SET @Message = CONCAT(@Message, 'Product with ID ', @ProductID, ' already has a DiscontinuedDate set. ', CHAR(10))
        END

        FETCH NEXT FROM ProductCursor INTO @ProductID
    END

    CLOSE ProductCursor
    DEALLOCATE ProductCursor

    PRINT @Message
END
GO


-- test
SET IDENTITY_INSERT SalesLT.Product ON;
GO

INSERT INTO SalesLT.Product (ProductID, DiscontinuedDate, Name, ProductNumber, StandardCost, ListPrice, SellStartDate) VALUES
    (1, NULL, 'Test product 1', '1', 1, 1, '2019-01-01'),
    (2, NULL, 'Test product 2', '2', 1, 1, '2019-01-01'),
    (3, NULL, 'Test product 3', '3', 1, 1, '2019-01-01'),
    (4, '1999-01-01', 'Test product 4', '4', 1, 1, '2019-01-01'),
    (5, NULL, 'Test product 5', '5', 1, 1, '2019-01-01'),
    (6, NULL, 'Test product 6', '6', 1, 1, '2019-01-01'),
    (7, '2010-01-01', 'Test product 7', '7', 1, 1, '2019-01-01'),
    (8, NULL, 'Test product 8', '8', 1, 1, '2019-01-01'),
    (9, '1998-01-01', 'Test product 9', '9', 1, 1, '2019-01-01'),
    (10, NULL, 'Test product 10', '10', 1, 1, '2019-01-01')
GO

SET IDENTITY_INSERT SalesLT.Product OFF;
GO

SELECT ProductID, DiscontinuedDate FROM SalesLT.Product WHERE ProductID < 11
GO

DECLARE @ProductIDs NVARCHAR(max) = '1,2,3,4,5,6,7,8,9,10'
EXEC UpdateProductDiscontinuedDate @ProductIDs = '1,2,3,4,5,6,7,8,9,10', @DiscontinuedDate = '2137-01-01'
GO

SELECT ProductID, DiscontinuedDate FROM SalesLT.Product WHERE ProductID < 11
GO

DELETE FROM SalesLT.Product WHERE ProductID < 11
GO
