from DB import *

# ------------------------------
import argparse

# create the parser
parser = argparse.ArgumentParser(description="Manage events and attendees")

# define the commands
subparsers = parser.add_subparsers(dest='command')

# 'add' command
add_parser = subparsers.add_parser('add', help='Add a new entry')
add_parser.add_argument('table', help='Name of table to add new entry to')
add_parser.add_argument('--values', required=False, nargs='+', help='Pairs of initial columns and values')

# 'delete' command
delete_parser = subparsers.add_parser('delete', help='Delete an existing entry')
delete_parser.add_argument('table', help='Name of table to delete entry from')
delete_parser.add_argument('--ids', required=False, nargs='+', help='IDs of entry to delete')
delete_parser.add_argument('--values', required=False, nargs='+', help='Pairs of column with values to delete')

# 'update' command
update_parser = subparsers.add_parser('update', help='Update an existing entry')
update_parser.add_argument('table', help='Name of table to update entry in')
update_parser.add_argument('--ids', required=False, nargs='+', help='IDs of entry to update')
update_parser.add_argument('--values', required=False, nargs='+', help='Pairs of column and value to update from')
update_parser.add_argument('--newvalues', required=True, nargs='+', help='Pairs of column and value to update to')

# 'search' command
search_parser = subparsers.add_parser('search', help='Search for an entry')
search_parser.add_argument('table', help='Name of table to search in')
search_parser.add_argument('--values', required=True, nargs='+', help='Pairs of column and value to search for')
search_parser.add_argument('--columns', required=False, nargs='+', help='Columns to return')


# ------------------------------
args = parser.parse_args()

if args.command == 'add':
    new_entry = None
    if(args.table == 'events'):
        new_entry = Event()
    elif(args.table == 'attendees'):
        new_entry = Attendee()
    elif(args.table == 'event_types'):
        new_entry = EventTypes()
    elif(args.table == 'event_attendees'):
        new_entry = EventAttendees()

    if(args.values):
        if(len(args.values) % 2 != 0):
            parser.error("Values must be in pairs")

        columns = args.values[::2]
        values = args.values[1::2]
        for i in range(len(columns)):
            setattr(new_entry, columns[i], values[i])

    session.add(new_entry)
    session.commit()

elif args.command == 'delete':
    entries = []
    if(args.ids):
        if(args.table == 'events'):
            entries += session.query(Event).filter(Event.id.in_(args.ids)).all()
        elif(args.table == 'attendees'):
            entries += session.query(Attendee).filter(Attendee.id.in_(args.ids)).all()
        elif(args.table == 'event_types'):
            entries += session.query(EventTypes).filter(EventTypes.id.in_(args.ids)).all()
        elif(args.table == 'event_attendees'):
            entries += session.query(EventAttendees).filter(EventAttendees.id.in_(args.ids)).all()

    if(args.values):
        if(len(args.values) % 2 != 0):
            parser.error("Values must be in pairs")

        columns = args.values[::2]
        values = args.values[1::2]

        if(args.table == 'events'):
            for i in range(len(columns)):
                entries += session.query(Event).filter(getattr(Event, columns[i]) == values[i]).all()
        elif(args.table == 'attendees'):
            for i in range(len(columns)):
                entries += session.query(Attendee).filter(getattr(Attendee, columns[i]) == values[i]).all()
        elif(args.table == 'event_types'):
            for i in range(len(columns)):
                entries += session.query(EventTypes).filter(getattr(EventTypes, columns[i]) == values[i]).all()
        elif(args.table == 'event_attendees'):
            for i in range(len(columns)):
                entries += session.query(EventAttendees).filter(getattr(EventAttendees, columns[i]) == values[i]).all()

    for entry in entries:
        session.delete(entry)
    session.commit()

elif args.command == 'update':
    entries = []
    if(args.ids):
        if(args.table == 'events'):
            entries += session.query(Event).filter(Event.id.in_(args.ids)).all()
        elif(args.table == 'attendees'):
            entries += session.query(Attendee).filter(Attendee.id.in_(args.ids)).all()
        elif(args.table == 'event_types'):
            entries += session.query(EventTypes).filter(EventTypes.id.in_(args.ids)).all()
        elif(args.table == 'event_attendees'):
            entries += session.query(EventAttendees).filter(EventAttendees.id.in_(args.ids)).all()

    if(args.values):
        if(len(args.values) % 2 != 0):
            parser.error("Values must be in pairs")
            
        columns = args.values[::2]
        values = args.values[1::2]

        if(args.table == 'events'):
            for i in range(len(columns)):
                entries += session.query(Event).filter(getattr(Event, columns[i]) == values[i]).all()
        elif(args.table == 'attendees'):
            for i in range(len(columns)):
                entries += session.query(Attendee).filter(getattr(Attendee, columns[i]) == values[i]).all()
        elif(args.table == 'event_types'):
            for i in range(len(columns)):
                entries += session.query(EventTypes).filter(getattr(EventTypes, columns[i]) == values[i]).all()
        elif(args.table == 'event_attendees'):
            for i in range(len(columns)):
                entries += session.query(EventAttendees).filter(getattr(EventAttendees, columns[i]) == values[i]).all()

    for entry in entries:
        columns = args.newvalues[::2]
        values = args.newvalues[1::2]
        for i in range(len(columns)):
            setattr(entry, columns[i], values[i])

    session.commit()

elif args.command == 'search':
    entries = []

    if(args.values):
        if(len(args.values) % 2 != 0):
            parser.error("Values must be in pairs")

        columns = args.values[::2]
        values = args.values[1::2]

        if(args.table == 'events'):
            for i in range(len(columns)):
                entries += session.query(Event).filter(getattr(Event, columns[i]) == values[i]).all()
        elif(args.table == 'attendees'):
            for i in range(len(columns)):
                entries += session.query(Attendee).filter(getattr(Attendee, columns[i]) == values[i]).all()
        elif(args.table == 'event_types'):
            for i in range(len(columns)):
                entries += session.query(EventTypes).filter(getattr(EventTypes, columns[i]) == values[i]).all()
        elif(args.table == 'event_attendees'):
            for i in range(len(columns)):
                entries += session.query(EventAttendees).filter(getattr(EventAttendees, columns[i]) == values[i]).all()

    for entry in entries:
        columns = args.columns
        if not args.columns:
            columns = [c.name for c in entry.__table__.columns]

        for column in columns:
            print(getattr(entry, column), end=' ')
        print()


# ------------------------------
# example (assuming the database is empty and indexed from 1):
"""
python zad.py add event_types --values id 1 name 'type1'
python zad.py add event_types --values id 2 name 'type2'

python zad.py add attendees --values name 'att1' email 'att1@emial.com'
python zad.py add attendees --values name 'att2' email 'att2@email.com'
python zad.py add attendees --values name 'att3' email 'att3@email.com'


python zad.py add events --values start_time '2021-01-01 12:00:00' end_time '2021-01-01 13:00:00' description 'event1' event_type_id 1
python zad.py add event_attendees --values event_id 1 attendee_id 1
python zad.py add event_attendees --values event_id 1 attendee_id 2

python zad.py add events --values start_time '2021-01-01 14:00:00' end_time '2021-01-01 15:00:00' description 'event2' event_type_id 2
python zad.py add event_attendees --values event_id 2 attendee_id 1
python zad.py add event_attendees --values event_id 2 attendee_id 3

python zad.py add events --values start_time '2021-01-01 12:00:00' end_time '2021-01-01 17:00:00' description 'event3' event_type_id 2

python zad.py search events --values event_type_id 2 --columns id start_time end_time description
python zad.py search events --values event_type_id 1
"""
