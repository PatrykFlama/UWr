[(back)](../)

# List 5
| 1 | 2 | 3 | 4 | 5 |
|---|---|---|---|---|
| X |   | ~ | ~ |   |


A transaction in SQL is a single unit of work that consists of one or more operations (like insert, update, delete). The property of a transaction is that it is atomic, which means either all of its operations are executed or none of them are.

Transactions have four key properties, often referred to as ACID properties:

* Atomicity: Ensures that all operations within the transaction are completed successfully; if not, the transaction is aborted at the failure point and previous operations are rolled back to their former state.
* Consistency: Ensures that the database properly changes states upon a successfully committed transaction.
* Isolation: Enables transactions to operate independently of and transparent to each other.
* Durability: Ensures that the result or effect of a committed transaction persists in case of a system failure.
In your SQL code, transactions are being used to ensure that the deletion operation is successful. If it's not, the changes are rolled back.

## Task 1
A savepoint in a DBMS is a way to mark a specific point in a transaction that you can roll back to without affecting the entire transaction. This is useful in large transactions where you might want to save your progress at certain points, so if an error occurs later, you can roll back to the savepoint instead of starting the transaction from the beginning. Savepoints can be nested, meaning you can have savepoints within savepoints.

## [Task 2](https://sqlchris.wordpress.com/2017/06/25/poziomy-izolacji-transakcji-sql-server/)
* Dirty Read: This occurs when a transaction reads data that has been written by another running transaction but not yet committed. If the other transaction rolls back, the read data becomes invalid.
* Non-Repeatable Read: This occurs when a transaction reads the same row twice, and gets different data each time. This happens when another transaction modifies the data between the two reads.
* Phantom Read: This occurs when a transaction re-executes a query returning a set of rows that satisfy a search condition and finds that the set of rows satisfying the condition has changed due to another recently-committed transaction.

These anomalies are controlled using different levels of transaction isolation. The SQL standard defines four levels: 
* Read Uncommitted: This is the lowest level of isolation. It allows dirty reads, non-repeatable reads, and phantom reads. One transaction may see uncommitted changes from another transaction.
* Read Committed: This level prevents dirty reads but allows non-repeatable reads and phantom reads. One transaction won't see uncommitted changes from other transactions, but data can change between multiple reads within the same transaction.
* Repeatable Read: This level prevents dirty reads and non-repeatable reads but allows phantom reads. Once a transaction reads data, other transactions can't change that data. However, other transactions can insert new data, causing a phantom read.
* Serializable: This is the highest level of isolation. It prevents dirty reads, non-repeatable reads, and phantom reads. It fully isolates one transaction from others. It's as if transactions queue up and execute one at a time.

