SELECT DISTINCT City FROM 
[SalesLT].[SalesOrderHeader] LEFT JOIN [SalesLT].[Address] ON ShipToAddressID = AddressID
ORDER BY 1