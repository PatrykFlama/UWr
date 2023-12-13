from pymongo import MongoClient
from datetime import datetime

client = MongoClient('localhost', 27017)

db = client['library']

books = db['books']
readers = db['readers']
borrowings = db['borrowings']


books_schema = {
    '$jsonSchema': {
        'bsonType': 'object',
        'required': ['title', 'author', 'isbn', 'copies'],
        'properties': {
            'title': {
                'bsonType': 'string',
                'description': 'must be a string and is required'
            },
            'author': {
                'bsonType': 'string',
                'description': 'must be a string and is required'
            },
            'isbn': {
                'bsonType': 'string',
                'description': 'must be a string and is required'
            },
            'copies': {
                'bsonType': 'array',
                'description': 'must be an array and is required',
                'items': {
                    'bsonType': 'object',
                    'required': ['copy_number', 'available'],
                    'properties': {
                        'copy_number': {
                            'bsonType': 'int',
                            'description': 'must be an integer and is required'
                        },
                        'available': {
                            'bsonType': 'bool',
                            'description': 'must be a boolean and is required'
                        }
                    }
                }
            }
        }
    }
}

# Validation schema for readers
readers_schema = {
    '$jsonSchema': {
        'bsonType': 'object',
        'required': ['name', 'email'],
        'properties': {
            'name': {
                'bsonType': 'string',
                'description': 'must be a string and is required'
            },
            'email': {
                'bsonType': 'string',
                'description': 'must be a string and is required'
            }
        }
    }
}

# Validation schema for borrowings
borrowings_schema = {
    '$jsonSchema': {
        'bsonType': 'object',
        'required': ['book_id', 'reader_id', 'borrow_date'],
        'properties': {
            'book_id': {
                'bsonType': 'objectId',
                'description': 'must be an objectId and is required'
            },
            'reader_id': {
                'bsonType': 'objectId',
                'description': 'must be an objectId and is required'
            },
            'borrow_date': {
                'bsonType': 'date',
                'description': 'must be a date and is required'
            },
            'return_date': {
                'bsonType': 'date',
                'description': 'must be a date'
            }
        }
    }
}

valAct = 'warn' # warn, error, or off
# Apply the validation schema to the collections (collMod = collection modify)
db.command({"collMod": "books", "validator": books_schema, "validationAction": valAct})
db.command({"collMod": "readers", "validator": readers_schema, "validationAction": valAct})
db.command({"collMod": "borrowings", "validator": borrowings_schema, "validationAction": valAct})
