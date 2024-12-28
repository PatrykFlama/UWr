using MediatR;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Domain.Logon
{
    public class LogonNotification : INotification
    {
        public string Username { get; set; }
    }

    public class LogonNotificationHandler : INotificationHandler<LogonNotification>
    {
        public Task Handle( LogonNotification notification, CancellationToken cancellationToken )
        {
            Console.WriteLine( $"Zalogowano: {notification.Username}" );

            return Task.CompletedTask;
        }
    }
}
