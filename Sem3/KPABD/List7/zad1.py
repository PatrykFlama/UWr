from pymongo import MongoClient

client = MongoClient('localhost', 27017)

library_db = client['library']

books_collection = library_db["books"]
books_collection.delete_many({})
books_collection.insert_many([
    {"title": "Book1", "author": "Author1", "year": 2020},
    {"title": "Book2", "author": "Author2", "year": 2021},
    {"title": "Book3", "author": "Author3", "year": 2022}
])

print("Zawartość kolekcji 'books':")
for book in books_collection.find():
    print(f"Title: {book['title']}, Author: {book['author']}, Year: {book['year']}")
