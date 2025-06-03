using MVP.Example.Refactored.Model;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MVP.Example.Refactored
{
    public interface ISubscriber<T>
    {
        void Handle(T Notification);
    }

    public class EventAggregator 
    {
        public static EventAggregator Instance = new EventAggregator();

        Dictionary<Type, List<object>> _subscribers = new Dictionary<Type, List<object>>();

        #region IEventAggregator Members

        public void AddSubscriber<T>(ISubscriber<T> Subscriber)
        {
            if (!_subscribers.ContainsKey(typeof(T)))
                _subscribers.Add(typeof(T), new List<object>());

            _subscribers[typeof(T)].Add(Subscriber);
        }

        public void RemoveSubscriber<T>(ISubscriber<T> Subscriber)
        {
            if (_subscribers.ContainsKey(typeof(T)))
                _subscribers[typeof(T)].Remove(Subscriber);
        }

      
        public void Publish<T>(T Event)
        {
            if (_subscribers.ContainsKey(typeof(T)))
                foreach (ISubscriber<T> subscriber in
                _subscribers[typeof(T)].OfType<ISubscriber<T>>())
                    subscriber.Handle(Event);
        }

        #endregion
    }

    public class PersonAddedNotification
    {
        public Person NewPerson { get; set; }
    }
}
