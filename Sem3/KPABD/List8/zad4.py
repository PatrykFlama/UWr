from neo4j import GraphDatabase
from secrets import URI, AUTH
from prettytable import PrettyTable

def print_persons(tx):
    result = tx.run("MATCH (p:Person) RETURN p.name AS name")
    table = PrettyTable(['Name'])
    for record in result:
        table.add_row([record['name']])
    print(table)


with GraphDatabase.driver(URI, auth=AUTH) as driver:
    driver.verify_connectivity()

    with driver.session() as session:
        session.read_transaction(print_persons)
