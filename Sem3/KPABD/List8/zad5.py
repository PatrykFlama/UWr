from neo4j import GraphDatabase
from secrets import URI, AUTH

class Person:
    def __init__(self, name):
        self.name = name

class DataBase:
    def __init__(self, URI, AUTH):
        self.driver = GraphDatabase.driver(URI, auth=AUTH)
    
    def __enter__(self):
        return self
    
    def __exit__(self, exc_type, exc_value, exc_traceback):
        self.close()

    def close(self):
        self.driver.close()

    def add_person(self, person):
        self.driver.execute_query(
            'CREATE (a: Person {name: $name})',
            name=person.name
        )

    def get_people(self):
        records, summary, keys = self.driver.execute_query(
            'MATCH (a: Person) RETURN a.name AS name'
        )
        return [Person(record['name']) for record in records]
    
    def delete_person(self, person):
        self.driver.execute_query(
            'MATCH (a: Person {name: $name}) DELETE a',
            name=person.name
        )
    def update_person(self, person, new_name):
        self.driver.execute_query(
            'MATCH (a: Person {name: $name}) SET a.name = $new_name',
            name=person.name,
            new_name=new_name
        )
    

with DataBase(URI, AUTH) as driver:
    driver.driver.verify_connectivity()

    driver.add_person(Person('Person'))
    driver.add_person(Person('Person1'))
    driver.update_person(Person('Person'), 'Person2')
    actors = driver.get_people()

    print('with addition:')
    for person in actors:
        print(person.name)

    driver.delete_person(Person('Person2'))
    actors = driver.get_people()

    print('with deletion:')
    for person in actors:
        print(person.name)
