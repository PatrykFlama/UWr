SELECT DISTINCT City 
FROM [SalesLT].[Address]
WHERE AddressID IN (SELECT ShipToAddressID FROM [SalesLT].[SalesOrderHeader])
AND AddressID NOT IN (
    SELECT ShipToAddressID 
    FROM [SalesLT].[SalesOrderHeader]
    WHERE ShipDate IS NULL)
ORDER BY 1