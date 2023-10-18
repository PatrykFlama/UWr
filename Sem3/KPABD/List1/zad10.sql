-- CREATE TABLE M1(
--     K INT PRIMARY KEY,
--     V VARCHAR(20) NOT NULL
-- )

-- CREATE TABLE S1(
--     K INT PRIMARY KEY, 
--     MFK INT FOREIGN KEY REFERENCES M1(K) ON UPDATE CASCADE ON DELETE SET NULL, 
--     V VARCHAR(20)
-- )

-- CREATE TABLE M2(
--     K1 INT,
--     K2 INT,
--     PRIMARY KEY (K1, K2),
--     V VARCHAR(20)
-- )

-- CREATE TABLE S2(
--     K INT PRIMARY KEY, 
--     MFK1 INT, 
--     MFK2 INT,
--     FOREIGN KEY (MFK1, MFK2) REFERENCES M2(K1, K2) ON UPDATE NO ACTION ON DELETE CASCADE,
--     V VARCHAR(20)
-- )


-- INSERT INTO M1 (K, V) VALUES (1, 'Value M1 1'), (2, 'Value M1 2'), (3, 'Value M1 3');
-- INSERT INTO S1 (K, MFK, V) VALUES (1, 1, 'Value S1 1'), (2, 2, 'Value S1 2'), (3, 3, 'Value S1 3');
-- INSERT INTO M2 (K1, K2, V) VALUES (1, 1, 'Value M2 1'), (2, 2, 'Value M2 2'), (3, 3, 'Value M2 3');
-- INSERT INTO S2 (K, MFK1, MFK2, V) VALUES (1, 1, 1, 'Value S2 1'), (2, 2, 2, 'Value S2 2'), (3, 3, 3, 'Value S2 3');


-- test foreign key 
INSERT INTO S1 (K, MFK, V) VALUES (4, 4, 'Value S1 4');
-- ERROR: The INSERT statement conflicted with the FOREIGN KEY constraint "FK__S1__MFK__56E8E7AB". The conflict occurred in database "AdvWorksLT", table "dbo.M1", column 'K'.
INSERT INTO S2 (K, MFK1, MFK2, V) VALUES (4, 4, 4, 'Value S2 4');
-- ERROR: The INSERT statement conflicted with the FOREIGN KEY constraint "FK__S2__5BAD9CC8". The conflict occurred in database "AdvWorksLT", table "dbo.M2".

-- UPDATE M1 SET K = 4 WHERE K = 1;
-- INSERT INTO S1 (K, MFK, V) VALUES (4, 4, 'Value S1 4');


-- ALTER TABLE table_name
-- ADD CONSTRAINT constraint_name constraint_definition;
-- for egzample
-- ALTER TABLE S1
-- ADD CONSTRAINT FK_S1_M1
-- FOREIGN KEY (MFK) REFERENCES M1(K) ON UPDATE CASCADE ON DELETE SET NULL;


-- ON UPDATE and ON DELETE clauses
-- the NO ACTION option prevents the action from being taken, 
-- the SET NULL option sets the foreign key value to null, 
-- and the CASCADE option updates the foreign key value to match the new value in the referenced table.


SELECT * FROM M1;
SELECT * FROM S1;
SELECT * FROM M2;
SELECT * FROM S2;