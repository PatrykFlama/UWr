SELECT h.SalesOrderID, h.SalesOrderNumber, h.PurchaseOrderNumber, SUM(LineTotal) AS TotalAfterDiscount, SUM(LineTotal) + SUM(UnitPrice*UnitPriceDiscount*OrderQty) AS TotalBeforeDiscount, SUM(OrderQty) AS TotalQuantity
FROM [SalesLT].[SalesOrderDetail] d LEFT JOIN [SalesLT].[SalesOrderHeader] h ON d.SalesOrderID = h.SalesOrderID
GROUP BY h.SalesOrderID, h.SalesOrderNumber, h.PurchaseOrderNumber
ORDER BY SUM(UnitPrice*UnitPriceDiscount*OrderQty) DESC

-- according to documentation, linetotal is total price with discount included