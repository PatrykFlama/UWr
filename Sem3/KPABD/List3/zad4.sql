DROP TABLE IF EXISTS Employees;
DROP TABLE IF EXISTS SalaryHistory;

CREATE TABLE Employees(
    ID INT PRIMARY KEY,
    SalaryGros DECIMAL(10, 2)
);

CREATE TABLE SalaryHistory(
    ID INT PRIMARY KEY,
    EmployeeID INT,
    Year INT,
    Month INT,
    SalaryNet DECIMAL(10, 2),
    SalaryGros DECIMAL(10, 2),

    FOREIGN KEY (EmployeeID) REFERENCES Employees(ID)
);

INSERT INTO Employees
    (ID, SalaryGros)
VALUES
    (1, 5000.00),
    (2, 6000.00),
    (3, 4500.00),
    (4, 10.00);

DECLARE @Year INT = 2023;
DECLARE @Months TABLE (MonthNum INT);
INSERT INTO @Months (MonthNum)
VALUES (1), (2), (3), (4), (5), (6), (7), (8), (9), (10), (11), (12);

INSERT INTO SalaryHistory
    (ID, EmployeeID, Year, Month, SalaryNet, SalaryGros)
SELECT
    ROW_NUMBER() OVER (ORDER BY E.ID, M.MonthNum) AS ID,
    E.ID AS EmployeeID,
    @Year AS Year,
    M.MonthNum AS Month,
    CAST(ROUND(E.SalaryGros * 0.8, 2) AS DECIMAL(18, 2)) AS SalaryNet, -- Assuming net is 80% of gross
    E.SalaryGros
FROM Employees E
CROSS JOIN @Months M;

DELETE FROM SalaryHistory
FROM SalaryHistory SH
    JOIN Employees E ON SH.EmployeeID = E.ID
WHERE SH.EmployeeID = 1 AND SH.Year = @Year AND SH.Month < 5;

DELETE FROM SalaryHistory
FROM SalaryHistory SH
    JOIN Employees E ON SH.EmployeeID = E.ID
WHERE SH.EmployeeID = 4 AND SH.Year = @Year AND SH.Month = 3;
GO

--------------------------------------------
DROP PROCEDURE IF EXISTS ComputeSalary
GO

CREATE PROCEDURE ComputeSalary
    @Month INT
AS
BEGIN
    IF @Month < 1 OR @Month > 12
    BEGIN
        PRINT 'Invalid month';
        RETURN;
    END

    DECLARE @sum DECIMAL(10, 2) = 0, @log BIT = 'false';
    DECLARE @last_id INT = -1, @last_month INT = 0;
    DECLARE @log_table TABLE (EmployeeID INT)
    DECLARE @salary_table TABLE (EmployeeID INT,
        PrevMonthsPaid DECIMAL(10, 2),
        ThisMonthToPay DECIMAL(10, 2),
        AllMonthsTax DECIMAL(10, 2));

    DECLARE SalaryCursor CURSOR FOR
        SELECT EmployeeID, Month, SalaryGros
    FROM SalaryHistory
    WHERE Month < @Month
    ORDER BY EmployeeID, Month;

    DECLARE @EmployeeID INT, @ActMonth DECIMAL(10, 2), @SalaryGros DECIMAL(10, 2);
    OPEN SalaryCursor;
    FETCH NEXT FROM SalaryCursor INTO @EmployeeID, @ActMonth, @SalaryGros;

    WHILE @@FETCH_STATUS = 0
    BEGIN
        IF @EmployeeID <> @last_id
        BEGIN
            -- new employee <=> save results and reset
            IF @last_id = -1    -- skip first employee
            BEGIN
                SET @last_id = @EmployeeID;
                CONTINUE
            END

            IF @log = 'true'
            BEGIN
                INSERT INTO @log_table
                    (EmployeeID)
                VALUES
                    (@last_id);
            END
            ELSE 
            BEGIN
                DECLARE @employee_month_salary DECIMAL(10, 2);
                SELECT @employee_month_salary = SalaryGros
                FROM Employees
                WHERE ID = @last_id;
                -- SET @sum = @sum + @employee_month_salary;

                DECLARE @tax DECIMAL(10, 2);
                IF @sum > 120000.00
                BEGIN
                    SET @tax = (15300 + 0.32 * @sum)
                END
                ELSE
                BEGIN
                    SET @tax = 0.17 * @sum
                END

                INSERT INTO @salary_table
                    (EmployeeID, PrevMonthsPaid, ThisMonthToPay, AllMonthsTax)
                VALUES
                    (@last_id, @sum, @employee_month_salary, @tax);
            END

            -- reset variables
            SET @sum = 0;
            SET @log = 'false';
            SET @last_month = 0;
            SET @last_id = @EmployeeID;

            CONTINUE;
        END

        IF @ActMonth = @last_month + 1
        BEGIN
            -- if the month is progressing
            SET @sum = @sum + @SalaryGros;
            SET @last_month = @ActMonth;
        END
        ELSE
        BEGIN
            -- if we had a jump thru months
            IF @last_month <> 0    -- and its not first jump <=> not freshly recruited
            BEGIN
                SET @log = 'true';
            END
            ELSE
            BEGIN
                SET @sum = @sum + @SalaryGros;
                SET @last_month = @ActMonth;
            END
        END

        FETCH NEXT FROM SalaryCursor INTO @EmployeeID, @ActMonth, @SalaryGros;
    END

    /* #region  */
    IF @log = 'true'
    BEGIN
        INSERT INTO @log_table
            (EmployeeID)
        VALUES
            (@last_id);
    END
    ELSE 
    BEGIN
        DECLARE @employee_month_salary_out DECIMAL(10, 2);
        SELECT @employee_month_salary_out = SalaryGros
        FROM Employees
        WHERE ID = @last_id;
        -- SET @sum = @sum + @employee_month_salary_out;

        DECLARE @tax_out DECIMAL(10, 2);
        IF @sum > 120000.00
        BEGIN
            SET @tax_out = (15300 + 0.32 * @sum)
        END
        ELSE
        BEGIN
            SET @tax_out = 0.17 * @sum
        END

        INSERT INTO @salary_table
            (EmployeeID, PrevMonthsPaid, ThisMonthToPay, AllMonthsTax)
        VALUES
            (@last_id, @sum, @employee_month_salary_out, @tax_out);
    END
    /* #endregion */

    CLOSE SalaryCursor;
    DEALLOCATE SalaryCursor;

    SELECT *
    FROM @salary_table;
    SELECT *
    FROM @log_table;
END
GO

---------------------------------------------------------------------------------
EXEC ComputeSalary 6
SELECT EmployeeID, Month, SalaryGros
FROM SalaryHistory
WHERE Month < 6
ORDER BY EmployeeID, Month;
