from flask import Flask, request, jsonify
from flask_sqlalchemy import SQLAlchemy
from secrets import username, password, host
import DB


app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = f'mysql+mysqlconnector://{username}:{password}@{host}/pythoncourse'
db = SQLAlchemy(app)

@app.route('/events', methods=['GET'])
def get_events():
    events = []
    for event in DB.session.query(DB.Event).all():
        events.append({'id': event.id, 'start_time': event.start_time, 'end_time': event.end_time, 'description': event.description, 'event_type_id': event.event_type_id})
    return jsonify(events)

@app.route('/events', methods=['POST'])
def create_event():
    event = DB.Event(start_time=request.json['start_time'], end_time=request.json['end_time'], description=request.json['description'], event_type_id=request.json['event_type_id'])
    DB.session.add(event)
    DB.session.commit()
    return jsonify({'message': 'Event created successfully'})

@app.route('/events/<int:event_id>', methods=['GET'])
def get_event_by_id(event_id):
    event = DB.session.query(DB.Event).filter(DB.Event.id == event_id).first()
    if event:
        return jsonify({'id': event.id, 'start_time': event.start_time, 'end_time': event.end_time, 'description': event.description, 'event_type_id': event.event_type_id})
    else:
        return jsonify({'message': 'Event not found'})

@app.route('/events/<int:event_id>', methods=['PUT'])
def update_event(event_id):
    event = DB.session.query(DB.Event).filter(DB.Event.id == event_id).first()
    if event:
        event.start_time = request.json['start_time']
        event.end_time = request.json['end_time']
        event.description = request.json['description']
        event.event_type_id = request.json['event_type_id']
        DB.session.commit()
        return jsonify({'message': 'Event updated successfully'})
    else:
        return jsonify({'message': 'Event not found'})

@app.route('/events/<int:event_id>', methods=['DELETE'])
def delete_event(event_id):
    event = DB.session.query(DB.Event).filter(DB.Event.id == event_id).first()
    if event:
        DB.session.delete(event)
        DB.session.commit()
        return jsonify({'message': 'Event deleted successfully'})
    else:
        return jsonify({'message': 'Event not found'})
    
if __name__ == '__main__':
    app.run()
