from sqlalchemy import create_engine, Column, Integer, String, DateTime, ForeignKey, and_, or_
from sqlalchemy.orm import declarative_base, relationship, validates
from sqlalchemy.orm.session import sessionmaker
from secrets import username, password, host

# create the database engine with debug logging
engine = create_engine(f'mysql+mysqlconnector://{username}:{password}@{host}/pythoncourse')#, echo=True)

# create a base class for declarative models
Base = declarative_base()

class Event(Base):
    __tablename__ = 'events'

    id = Column(Integer, primary_key=True)
    start_time = Column(DateTime)
    end_time = Column(DateTime)
    description = Column(String(100))
    event_type_id = Column(Integer, ForeignKey('event_types.id'))
    # event_type = relationship('EventTypes', back_populates='events')
    attendees = relationship('EventAttendees', backref='event_attendees')

    @validates('start_time', 'end_time')
    def validate_time(self, key, value):
        new_start = self.start_time
        new_end = self.end_time

        if key == 'start_time':
            new_start = value
        elif key == 'end_time':
            new_end = value

        if new_start and new_end:
            overlapping_event = session.query(Event).filter(
                Event.id != self.id,
                and_(
                    Event.start_time < new_end,
                    Event.end_time > new_start
                )
            ).first()
            assert overlapping_event is None, "Event time is overlapping with another event"
        return value


class Attendee(Base):
    __tablename__ = 'attendees'

    id = Column(Integer, primary_key=True)
    name = Column(String(100))
    email = Column(String(100))
    # event_attendees = relationship('EventAttendees', backref='event_attendees')

class EventTypes(Base):
    __tablename__ = 'event_types'

    id = Column(Integer, primary_key=True)
    name = Column(String(100))
    events = relationship('Event', backref='event_types')
    # events = relationship('Event', back_populates='event_type')

class EventAttendees(Base):
    __tablename__ = 'event_attendees'

    id = Column(Integer, primary_key=True)
    event_id = Column(Integer, ForeignKey('events.id'))
    attendee_id = Column(Integer, ForeignKey('attendees.id'))

# create the tables with __tablename__ attribute
Base.metadata.create_all(engine)

Session = sessionmaker(bind=engine)
session = Session()
