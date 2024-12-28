using MediatR;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Domain.Logon
{
    public class LogonUseCaseParameters : IRequest<LogonUseCaseResults>
    {
        public string Username { get; set; }
        public string Password { get; set; }
    }

    public class LogonUseCaseResults
    {
        public bool LogonStatus { get; set; }
    }

    public class LogonUseCaseHandler : IRequestHandler<LogonUseCaseParameters, LogonUseCaseResults>
    {
        public async Task<LogonUseCaseResults> Handle(
            LogonUseCaseParameters request,
            CancellationToken cancellationToken )
        {
            // tu logika

            var logonStatus = request.Username == request.Password;

            return new LogonUseCaseResults()
            {
                LogonStatus = logonStatus
            };
        }
    }

}
