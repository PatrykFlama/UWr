SELECT pc.Name, p.Name
FROM [SalesLT].[Product] p LEFT JOIN [SalesLT].[ProductCategory] pc ON p.ProductCategoryID = pc.ProductCategoryID 
WHERE ParentProductCategoryID IS NULL