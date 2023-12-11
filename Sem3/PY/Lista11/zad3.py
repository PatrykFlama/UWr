from sqlalchemy import create_engine, Column, Integer, String, DateTime, ForeignKey, and_
from sqlalchemy.orm import declarative_base, relationship, validates
from sqlalchemy.orm.session import sessionmaker
from secrets import username, password, host

# create the database engine with debug logging
engine = create_engine(f'mysql+mysqlconnector://{username}:{password}@{host}/pythoncourse', echo=True)

# create a base class for declarative models
Base = declarative_base()

class Event(Base):
    __tablename__ = 'events'

    id = Column(Integer, primary_key=True)
    start_time = Column(DateTime)
    end_time = Column(DateTime)
    description = Column(String)
    attendees = relationship('Attendee', back_populates='event')
    event_type_id = Column(Integer, ForeignKey('event_types.id'))
    event_type = relationship('EventTypes', back_populates='events')

    @validates('start_time', 'end_time')
    def validate_time(self, key, value):
        if self.start_time and self.end_time:
            overlapping_event = session.query(Event).filter(
                Event.id != self.id,
                and_(
                    Event.start_time < self.end_time,
                    Event.end_time > self.start_time
                )
            ).first()
            assert overlapping_event is None, "Event timeslot is overlapping with another event"
        return value


class Attendee(Base):
    __tablename__ = 'attendees'

    id = Column(Integer, primary_key=True)
    name = Column(String)
    email = Column(String)
    event_id = Column(Integer, ForeignKey('events.id'))
    event = relationship('Event', back_populates='attendees')

class EventTypes(Base):
    __tablename__ = 'event_types'

    id = Column(Integer, primary_key=True)
    name = Column(String)
    events = relationship('Event', back_populates='event_type')

# create the tables with __tablename__ attribute
Base.metadata.create_all(engine)

"""
# ------------------------------
import argparse

# create a session factory
Session = sessionmaker(bind=engine)
session = Session()

# create the parser
parser = argparse.ArgumentParser(description="Manage events and attendees")

# define the commands
subparsers = parser.add_subparsers(dest='command')

# 'add' command
add_parser = subparsers.add_parser('add', help='Add a new event')
add_parser.add_argument('--description', required=True, help='Description of the event')

# 'update' command
update_parser = subparsers.add_parser('update', help='Update an existing event')
update_parser.add_argument('--id', required=True, type=int, help='ID of the event to update')
update_parser.add_argument('--description', required=True, help='New description of the event')

# 'search' command
search_parser = subparsers.add_parser('search', help='Search for an event')
search_parser.add_argument('--description', required=True, help='Description of the event to search for')

# parse the arguments
args = parser.parse_args()



# handle the commands
if args.command == 'add':
    event = Event(description=args.description)
    session.add(event)
    session.commit()
elif args.command == 'update':
    event = session.query(Event).get(args.id)
    if event:
        event.description = args.description
        session.commit()
elif args.command == 'search':
    event = session.query(Event).filter(Event.description == args.description).first()
    if event:
        print(f'Found event: {event.description}')
    else:
        print('No event found')

"""