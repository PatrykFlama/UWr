import mysql.connector
from secrets import host, user, password

db = mysql.connector.connect(user=user, password=password, host=host, database="pythoncourse")
cursor = db.cursor()

cursor.execute("""
CREATE TABLE IF NOT EXISTS movies (
    id INT AUTO_INCREMENT PRIMARY KEY,
    title VARCHAR(255) NOT NULL,
    year INT NOT NULL,
    director VARCHAR(255) NOT NULL,
    cinematographer VARCHAR(255) NOT NULL,
    producer VARCHAR(255) NOT NULL
)
""")

def insert_movie(title, year, director, cinematographer, producer):
    insert_query = """
    INSERT INTO movies (title, year, director, cinematographer, producer)
    VALUES (%s, %s, %s, %s, %s)
    """
    values = (title, year, director, cinematographer, producer)
    cursor.execute(insert_query, values)
    db.commit()

