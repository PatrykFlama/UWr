SELECT pm.Name, COUNT(*) amt
FROM [SalesLT].[Product] p LEFT JOIN [SalesLT].[ProductModel] pm ON p.ProductModelID = pm.ProductModelID
GROUP BY pm.ProductModelID, pm.Name
HAVING COUNT(*) > 1

-- if we would be grouping only by pm.Name, if there would be two different product models with same name, they would be grouped together
