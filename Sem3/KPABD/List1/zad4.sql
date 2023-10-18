-- INSERT INTO [SalesLT].[ProductCategory] (Name, ParentProductCategoryID) VALUES ('MTB600', 5)
-- INSERT INTO [SalesLT].[Product] (Name, ProductCategoryID, ProductNumber, StandardCost, ListPrice, SellStartDate) VALUES ('MTB600 bike product', 42, 'MTB-600', 1000.00, 1500.00, '2014-02-01')

SELECT pc.Name, p.Name
FROM [SalesLT].[Product] p LEFT JOIN [SalesLT].[ProductCategory] pc
ON p.ProductCategoryID = pc.ProductCategoryID
WHERE pc.ProductCategoryID IN (SELECT ParentProductCategoryID FROM [SalesLT].[ProductCategory])
