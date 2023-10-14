-- ALTER TABLE SalesLT.Customer
-- ADD CreditCardNumber VARCHAR(20) NOT NULL DEFAULT '0000-0000-0000-0000';

-- SELECT TOP 10 * FROM SalesLT.Customer;

-- UPDATE TOP(3) SalesLT.SalesOrderHeader
-- SET CreditCardApprovalCode = '123456';

-- SELECT TOP 10 * FROM SalesLT.SalesOrderHeader;

-- UPDATE SalesLT.Customer
-- SET CreditCardNumber = 'X'
-- FROM SalesLT.Customer 
-- JOIN SalesLT.SalesOrderHeader
-- ON SalesLT.Customer.CustomerID = SalesLT.SalesOrderHeader.CustomerID
-- WHERE SalesLT.SalesOrderHeader.CreditCardApprovalCode IS NOT NULL;

SELECT Customer.CustomerID, CreditCardApprovalCode, CreditCardNumber FROM SalesLT.Customer LEFT JOIN SalesLT.SalesOrderHeader
ON SalesLT.Customer.CustomerID = SalesLT.SalesOrderHeader.CustomerID
WHERE CreditCardApprovalCode IS NOT NULL;