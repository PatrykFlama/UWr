from pymongo import MongoClient
from datetime import datetime

client = MongoClient('localhost', 27017)

db = client['library']

books = db['books']
books.delete_many({})
book1_id = books.insert_one({
    'title': 'Book 1',
    'author': 'Author 1',
    'isbn': 'ISBN1',
    'copies': [
        {'copy_number': 1, 'available': True},
        {'copy_number': 2, 'available': False},
        {'copy_number': 3, 'available': True}
    ]
}).inserted_id

book2_id = books.insert_one({
    'title': 'Book 2',
    'author': 'Author 2',
    'isbn': 'ISBN2',
    'copies': [
        {'copy_number': 1, 'available': True},
        {'copy_number': 2, 'available': True},
        {'copy_number': 3, 'available': False}
    ]
}).inserted_id

books.insert_many([
    {
        'title': 'Book 3',
        'author': 'Author 3',
        'isbn': 'ISBN3',
        'copies': [
            {'copy_number': 1, 'available': True},
        ]
    },
    {
        'title': 'Book 4',
        'author': 'Author 4',
        'isbn': 'ISBN4',
        'copies': [
            {'copy_number': 1, 'available': False},
            {'copy_number': 2, 'available': False},
        ]
    }
])

# Insert readers
readers = db['readers']
readers.delete_many({})
reader1_id = readers.insert_one({'name': 'Reader 1', 'email': 'reader1@example.com'}).inserted_id
reader2_id = readers.insert_one({'name': 'Reader 2', 'email': 'reader2@example.com'}).inserted_id

# Insert borrowings
borrowings = db['borrowings']
borrowings.delete_many({})
borrowings.insert_many([
    {'book_id': book1_id, 'reader_id': reader1_id, 'borrow_date': datetime.now(), 'return_date': None},
    {'book_id': book1_id, 'reader_id': reader2_id, 'borrow_date': datetime.now(), 'return_date': None},
    {'book_id': book2_id, 'reader_id': reader1_id, 'borrow_date': datetime.now(), 'return_date': None},
    {'book_id': book2_id, 'reader_id': reader2_id, 'borrow_date': datetime.now(), 'return_date': None}
])