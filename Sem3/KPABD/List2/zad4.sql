CREATE PROCEDURE get_borrowed_days_by_reader_id
    @reader_ids TABLE (id INT)
AS
BEGIN
    SELECT b.reader_id, SUM(DATEDIFF(day, b.borrow_date, b.return_date)) AS 'sum_days'
    FROM borrows b
    WHERE b.reader_id IN (SELECT id FROM @reader_ids)
    GROUP BY b.reader_id
END
