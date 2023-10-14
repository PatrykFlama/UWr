SELECT TOP 1 h.SalesOrderID, SalesOrderNumber, PurchaseOrderNumber, LineTotal, (LineTotal-UnitPrice*UnitPriceDiscount) AfterDiscount, OrderQty
FROM [SalesLT].[SalesOrderDetail] d LEFT JOIN [SalesLT].[SalesOrderHeader] h ON d.SalesOrderID = h.SalesOrderID
ORDER BY UnitPrice*UnitPriceDiscount DESC