-- to view the constraint we use [EXEC sp_helpconstraint 'table_name']:
EXEC sp_helpconstraint 'SalesLT.SalesOrderHeader';
-- EXEC sp_help 'SalesLT.SalesOrderHeader';     -- to view everything: constraints, indexes, triggers, etc.


-- Insert a row into the SalesOrderHeader table with a ShipDate earlier than the OrderDate
INSERT INTO SalesLT.SalesOrderHeader (OrderDate, ShipDate, DueDate, CustomerID, BillToAddressID, ShipToAddressID, Status, SubTotal, TaxAmt, Freight, Comment, ShipMethod)
VALUES ('2021-01-01', '2020-12-31', '2021-01-02', 1, 653, 1092, 1, 1, 1, 1, 'test', 1);
-- ERROR: The INSERT statement conflicted with the CHECK constraint "CK_SalesOrderHeader_ShipDate". The conflict occurred in database "AdvWorksLT", table "SalesLT.SalesOrderHeader".

-- to disable the constraint:
ALTER TABLE SalesLT.SalesOrderHeader NOCHECK CONSTRAINT CK_SalesOrderHeader_ShipDate;

-- Insert a row into the SalesOrderHeader table with a ShipDate earlier than the OrderDate
INSERT INTO SalesLT.SalesOrderHeader (OrderDate, ShipDate, DueDate, CustomerID, BillToAddressID, ShipToAddressID, Status, SubTotal, TaxAmt, Freight, Comment, ShipMethod)
VALUES ('2021-01-01', '2020-12-31', '2021-01-02', 1, 653, 1092, 1, 1, 1, 1, 'test', 1);

-- to enable the constraint:
ALTER TABLE SalesLT.SalesOrderHeader CHECK CONSTRAINT CK_SalesOrderHeader_ShipDate;


SELECT TOP 10 * FROM [SalesLT].[SalesOrderHeader]
DBCC CHECKCONSTRAINTS

DELETE FROM SalesLT.SalesOrderHeader WHERE OrderDate='2021-01-01' AND ShipDate='2020-12-31';

SELECT TOP 10 * FROM [SalesLT].[SalesOrderHeader]
DBCC CHECKCONSTRAINTS



-- to create a new constraint:
-- ALTER TABLE table_name
-- ADD CONSTRAINT constraint_name constraint_definition;