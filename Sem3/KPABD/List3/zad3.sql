DROP TABLE IF EXISTS Products;
DROP TABLE IF EXISTS Rates;
DROP TABLE IF EXISTS Prices;
GO

CREATE TABLE Products(
    ID INT PRIMARY KEY,
    ProductName VARCHAR(50),
);

CREATE TABLE Rates(
    Currency VARCHAR(3) PRIMARY KEY,
    PricePLN DECIMAL(10, 2),
);

CREATE TABLE Prices(
    ProductID INT,
    Currency VARCHAR(3),       -- references currency
    Price DECIMAL(10, 2),

    FOREIGN KEY (ProductID) REFERENCES Products(ID),
);
GO

-- Inserting sample products
INSERT INTO Products (ID, ProductName) VALUES
    (1, 'Product A'),
    (2, 'Product B'),
    (3, 'Product C');

-- Inserting sample rates
INSERT INTO Rates (Currency, PricePLN) VALUES
    ('USD', 3.75),
    ('EUR', 4.25),
    ('GBP', 4.90);

-- Inserting sample prices for products in different currencies
INSERT INTO Prices (ProductID, Currency, Price) VALUES
    (1, 'USD', 20.00),
    (1, 'EUR', 18.00),
    (1, 'PLN', 50.00),
    (2, 'USD', 15.50),
    (2, 'GBP', 12.75),
    (2, 'PLN', 60.00),
    (3, 'EUR', 30.00),
    (3, 'JPY', 5000.00),
    (3, 'PLN', 100.00);
GO

---------------------------------------------------------------------
DROP PROCEDURE IF EXISTS UpdatePrices
GO  

CREATE PROCEDURE UpdatePrices
AS
BEGIN 
    DECLARE PriceCursor CURSOR FOR
        SELECT ProductID, Currency, Price FROM Prices
        WHERE Currency NOT LIKE 'PLN'

    DECLARE @ProductID INT
    DECLARE @Currency VARCHAR(3)
    DECLARE @Price DECIMAL(10, 2)

    OPEN PriceCursor
    FETCH NEXT FROM PriceCursor INTO @ProductID, @Currency, @Price

    WHILE @@FETCH_STATUS = 0
    BEGIN
        DECLARE @RatePLN DECIMAL(10, 2) = NULL
        SELECT @RatePLN = PricePLN FROM Rates 
        WHERE Currency = @Currency

        IF @RatePLN IS NULL
        BEGIN
            DELETE FROM Prices
            WHERE ProductID = @ProductID AND Currency = @Currency
        END
        ELSE
        BEGIN
            DECLARE @PricePLN DECIMAL(10, 2)
            SELECT @PricePLN = Price FROM Prices 
            WHERE ProductID = @ProductID AND Currency = 'PLN'

            UPDATE Prices SET Price = @PricePLN / @RatePLN
            WHERE ProductID = @ProductID AND Currency = @Currency
        END

        FETCH NEXT FROM PriceCursor INTO @ProductID, @Currency, @Price
    END

    CLOSE PriceCursor
    DEALLOCATE PriceCursor
END
GO

---------------------------------------------------------------------
SELECT * FROM Prices
EXEC UpdatePrices
SELECT * FROM Prices

---------------------------------------------------------------------
-- DROP TABLE IF EXISTS Products;
-- DROP TABLE IF EXISTS Rates;
-- DROP TABLE IF EXISTS Prices;
-- DROP PROCEDURE IF EXISTS UpdatePrices
-- GO
