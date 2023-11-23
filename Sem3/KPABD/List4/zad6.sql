CREATE TABLE Cache(ID INT IDENTITY PRIMARY KEY, UrlAddress VARCHAR(150), LastAccess DATETIME)
CREATE TABLE History(ID INT IDENTITY PRIMARY KEY, UrlAddress VARCHAR(150), LastAccess DATETIME)
CREATE TABLE Parameters(nazwa VARCHAR(150), cache_size INT)
GO

INSERT INTO Parameters VALUES('max_cache', 4)     -- size of cache
GO

--------------------------------------------------------------
CREATE TRIGGER move_to_history 
ON Cache
INSTEAD OF INSERT
AS
BEGIN
	DECLARE @I_UrlAddress varchar(150), @I_LastAccess DATETIME;

	SELECT @I_UrlAddress=UrlAddress, @I_LastAccess=LastAccess 
    FROM INSERTED;

    -- udpate if exists
	IF EXISTS (SELECT 1 FROM Cache WHERE UrlAddress=@I_UrlAddress)
		UPDATE Cache 
        SET LastAccess=@I_LastAccess 
        WHERE UrlAddress=@I_UrlAddress;
	ELSE
	BEGIN
        -- calc rows
		DECLARE @Rows INT,  @MaxRows INT;

		SET @Rows = (SELECT COUNT(*) 
        FROM Cache);

		SET @MaxRows = (SELECT TOP 1 cache_size 
        FROM Parameters);

        -- insert if we have space in cache
		IF (@Rows < @MaxRows)
			INSERT INTO Cache 
            SELECT UrlAddress, LastAccess 
            FROM INSERTED
		ELSE
		BEGIN
            -- move to history
			DECLARE @B_ID INT, @B_AdresUrl varchar(150), @B_LastAccess DATETIME
			SELECT TOP 1 @B_ID=ID, @B_AdresUrl=UrlAddress, @B_LastAccess=LastAccess 
            FROM Cache ORDER BY LastAccess
			IF EXISTS (SELECT 1 FROM History WHERE UrlAddress=@B_AdresUrl)
				UPDATE History 
                SET LastAccess=@B_LastAccess 
                WHERE UrlAddress=@B_AdresUrl
			ELSE
				INSERT INTO History VALUES(@B_AdresUrl, @B_LastAccess)

            -- delete oldest and insert new
			DELETE FROM Cache WHERE ID=@B_ID
			INSERT INTO Cache SELECT UrlAddress, LastAccess FROM INSERTED
		END
	END
END
GO

--------------------------------------------------------------
INSERT INTO Cache VALUES('link1.com',   '01/01/2020 13:31:00')
INSERT INTO Cache VALUES('link1.com',   '01/01/2020 13:31:01')
INSERT INTO Cache VALUES('link2.com',   '01/01/2020 13:31')
INSERT INTO Cache VALUES('link3.com',	'01/01/2020 13:31')
INSERT INTO Cache VALUES('link4.com',   '01/01/2020 13:31')
INSERT INTO Cache VALUES('link5.com',   '01/01/2020 10:31')
INSERT INTO Cache VALUES('link6.com',	'01/01/2020 10:31')
INSERT INTO Cache VALUES('link7.com',   '01/01/1999 13:31')
INSERT INTO Cache VALUES('link8.com',   '01/01/2020 10:31')
INSERT INTO Cache VALUES('link9.com',	'01/01/2020 13:31')
INSERT INTO Cache VALUES('link10.com',  '01/01/2020 09:15')
INSERT INTO Cache VALUES('link11.com',	'01/01/2020 13:32')

SELECT * FROM Cache
SELECT * FROM History
GO

--------------------------------------------------------------
DROP TABLE IF EXISTS Cache
DROP TABLE IF EXISTS History
DROP TABLE IF EXISTS Parameters
DROP TRIGGER IF EXISTS move_to_history
GO
