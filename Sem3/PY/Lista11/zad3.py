import mysql.connector
from secrets import host, user, password

class Calendar:
    def __init__(self):
        self.db = mysql.connector.connect(user=user, password=password, host=host, database="pythoncourse")
        self.cursor = self.db.cursor()

        self.cursor.execute("""
        CREATE TABLE IF NOT EXISTS events (
            id INT AUTO_INCREMENT PRIMARY KEY,
            start_time DATETIME NOT NULL,
            end_time DATETIME NOT NULL,
            description VARCHAR(255) NOT NULL
        )
        """)
        self.cursor.execute("""
        CREATE TABLE IF NOT EXISTS assigned_people (
            id INT AUTO_INCREMENT PRIMARY KEY,
            event_id INT NOT NULL,
            name VARCHAR(255) NOT NULL,
            email VARCHAR(255) NOT NULL,
            
            FOREIGN KEY (event_id) REFERENCES events(id)
        )
        """)

    def add_event(self, start_time, end_time, description, assigned_people):
        if self.check_event_conflict(start_time, end_time):
            print("Event conflicts with an existing event.")
            return

        self.cursor.execute("INSERT INTO events (start_time, end_time, description) VALUES (%s, %s, %s)",
                            (start_time, end_time, description))
        event_id = self.cursor.lastrowid

        for person in assigned_people:
            self.cursor.execute("INSERT INTO assigned_people (event_id, name, email) VALUES (%s, %s, %s)",
                                (event_id, person['name'], person['email']))

        self.db.commit()
        print("Event added successfully.")

    def check_event_conflict(self, start_time, end_time):
        self.cursor.execute("SELECT * FROM events WHERE start_time < %s AND end_time > %s",
                            (end_time, start_time))
        conflicting_events = self.cursor.fetchall()
        return len(conflicting_events) > 0


calendar = Calendar()
calendar.add_event("2022-01-01 10:00:00", "2022-01-01 12:00:00", "Event", [
    {"name": "Name1", "email": "mail@mail.com"},
    {"name": "Name2", "email": "mail@mail.com"}
])
calendar.add_event("2022-01-01 11:00:00", "2022-01-01 13:00:00", "Event", [
    {"name": "Name3", "email": "mail@mail.com"},
    {"name": "Name4", "email": "mail@mail.com"}
])

