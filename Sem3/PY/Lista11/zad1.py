import mysql.connector
from secrets import host, user, password

db = mysql.connector.connect(user=user, password=password, host=host, database="pythoncourse")
cursor = db.cursor()

# cursor.execute("CREATE DATABASE pythoncourse")

# cursor.execute("SHOW DATABASES")
# for x in cursor:
#   print(x)


cursor.execute('''
CREATE TABLE IF NOT EXISTS books (
    id INT AUTO_INCREMENT PRIMARY KEY,
    author VARCHAR(255) NOT NULL,
    title VARCHAR(255) NOT NULL,
    year INT NOT NULL
)
''')

cursor.execute('''
CREATE TABLE IF NOT EXISTS friends (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) NOT NULL
)
''')

cursor.execute('''
CREATE TABLE IF NOT EXISTS borrowings (
    id INT AUTO_INCREMENT PRIMARY KEY,
    book_id INT NOT NULL,
    friend_id INT NOT NULL,
               
    FOREIGN KEY (book_id) REFERENCES books(id),
    FOREIGN KEY (friend_id) REFERENCES friends(id)
)
''')

def add_book(author, title, year):
    query = "INSERT INTO books (author, title, year) VALUES (%s, %s, %s)"
    values = (author, title, year)
    cursor.execute(query, values)
    db.commit()
    print("Book added successfully")

def borrow_book(book_id, friend_id):
    query = "INSERT INTO borrowings (book_id, friend_id) VALUES (%s, %s)"
    values = (book_id, friend_id)
    cursor.execute(query, values)
    db.commit()
    print("Book borrowed successfully")

def return_book(book_id, friend_id):
    query = "DELETE FROM borrowings WHERE book_id = %s AND friend_id = %s"
    values = (book_id, friend_id)
    cursor.execute(query, values)
    db.commit()
    print("Book returned successfully")


