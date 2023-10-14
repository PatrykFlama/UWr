SELECT a.City, COUNT(*) customers, COUNT(DISTINCT c.SalesPerson) salespersons
FROM ([SalesLT].[Customer] c INNER JOIN [SalesLT].[CustomerAddress] ca ON c.CustomerID = ca.CustomerID) 
	LEFT JOIN [SalesLT].[Address] a ON ca.AddressID = a.AddressID
GROUP BY a.City