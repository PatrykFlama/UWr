SELECT c.FirstName, c.LastName, SUM(UnitPrice*UnitPriceDiscount)
FROM ([SalesLT].[SalesOrderDetail] d LEFT JOIN [SalesLT].[SalesOrderHeader] h ON d.SalesOrderID = h.SalesOrderID) 
	LEFT JOIN [SalesLT].[Customer] c ON h.CustomerID = c.CustomerID
GROUP BY c.CustomerID, c.FirstName, c.LastName