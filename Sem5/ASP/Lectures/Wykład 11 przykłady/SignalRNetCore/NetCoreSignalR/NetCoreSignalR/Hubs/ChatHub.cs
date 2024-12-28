using Microsoft.AspNetCore.SignalR;

namespace NetCoreSignalR.Hubs
{
    public class ChatHub : Hub
    {
        public override Task OnConnectedAsync()
        {
            // identyfikator łączącego się klienta
            var clientId = this.Context.ConnectionId;

            // dodanie klienta do grupy i wysłanie komunikatu tylko do grupy
            //await this.Groups.AddToGroupAsync(clientId, "nazwa grupy");
            //await this.Clients.Group("nazwa grupy").SendAsync(...);

            return base.OnConnectedAsync(); 
        }

        public async Task SendMessage(string message)
        {
            // metoda wywołana po stronie klienta
            await Clients.All.SendAsync( "DisplayMessage", 
                this.Context.User.Identity.Name, 
                message);
        }
    }
}
