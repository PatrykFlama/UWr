SELECT DISTINCT City FROM 
[SalesLT].[SalesOrderHeader] LEFT JOIN [SalesLT].[Address] ON ShipToAddressID = AddressID
WHERE ShipDate IS NOT NULL
ORDER BY 1