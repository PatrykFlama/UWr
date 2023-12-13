from pymongo import MongoClient
from datetime import datetime

client = MongoClient('localhost', 27017)

db = client['library']

books = db['books']
readers = db['readers']
borrowings = db['borrowings']


# Get full documents list, sorted in an explicit order, and limited to the middle 2 items
# Assuming we're sorting by 'title' in ascending order and our collection has 5 documents
print('Sorted by title in ascending order, limited to the middle 2 items:')
books_size = books.count_documents({})
books_cursor = books.find().sort('title', 1).skip(books_size//2-1).limit(2)
for book in books_cursor:
    print(book)

# Get the documents list filtered with some condition applied on the nested structure
# Assuming we're filtering books where the first copy is available
print()
print('Books where the first copy is available:')
filtered_books_cursor = books.find({'copies.0.available': True})
for book in filtered_books_cursor:
    print(book)


