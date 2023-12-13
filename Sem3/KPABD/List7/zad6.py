"""
1. Start a MongoDB instance for each shard:
mongod --shardsvr --dbpath /data/shard1 --port 27018
mongod --shardsvr --dbpath /data/shard2 --port 27019

2. Start a MongoDB instance for the config server:
mongod --configsvr --dbpath /data/config --port 27020

3. Start a mongos instance:
mongos --configdb localhost:27020
"""


from pymongo import MongoClient
from datetime import datetime

client = MongoClient('localhost', 27017)

db = client['library']

books = db['books']
readers = db['readers']
borrowings = db['borrowings']

client.admin.command('enableSharding', 'library')

client.admin.command('shardCollection', 'library.books', key={'_id': 1})
client.admin.command('shardCollection', 'library.readers', key={'_id': 1})
client.admin.command('shardCollection', 'library.borrowings', key={'_id': 1})

