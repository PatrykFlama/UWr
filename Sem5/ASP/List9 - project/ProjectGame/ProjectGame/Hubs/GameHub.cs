using Microsoft.AspNetCore.SignalR;
using System.Collections.Concurrent;
using GameLogic;
using Microsoft.AspNetCore.Authorization;

namespace ProjectGame.Hubs
{
    public class GameHub : Hub
    {
        private static readonly ConcurrentDictionary<string, TicTacToeGame> Games = new();

        [Authorize]
        public async Task JoinGame(string gameId)
        {
            if(!Games.ContainsKey(gameId))
            {
                Games[gameId] = new TicTacToeGame();
            }

            var game = Games[gameId];

            if(game.Players.Count >= 2)
            {
                throw new HubException("This game room is full");
            }

            var nickname = Context.User.Identity.Name;
            if(!game.Players.Contains(nickname))
            {
                game.Players.Add(nickname);
            }

            if(game.Players.Count == 2)
            {
                game.CurrentPlayer = game.Players[0];
                game.CurrentPlayerRole = "X";
            }

            await Groups.AddToGroupAsync(nickname, gameId);
            await Clients.Group(gameId).SendAsync("PlayerJoined", game.Players);
            await Clients.Group(gameId).SendAsync("GameState", game.Board, game.CurrentPlayerRole);

            await Clients.Client(game.Players[0]).SendAsync("PlayerRole", "X");
            await Clients.Client(game.Players[1]).SendAsync("PlayerRole", "O");
        }

        [Authorize]
        public async Task MakeMove(string gameId, string srow, string scol)
        {
            int row = int.Parse(srow), col = int.Parse(scol); //TODO why?

            if(!Games.TryGetValue(gameId, out var game) || game.Players.Count < 2)
            {
                throw new HubException("Invalid game state");
            }

            if(game.CurrentPlayer != Context.ConnectionId)
            {
                throw new HubException("Not your turn");
            }

            if(!string.IsNullOrEmpty(game.Board[row][col]))
            {
                throw new HubException("Cell already occupied");
            }


            var symbol = game.Players[0] == Context.ConnectionId ? "X" : "O";
            game.Board[row][col] = symbol;
            game.CurrentPlayerRole = game.CurrentPlayerRole == "X" ? "O" : "X";
            await Clients.Group(gameId).SendAsync("MoveMade", row, col, symbol, game.CurrentPlayerRole);

            var winner = game.CheckWinner();
            if(!string.IsNullOrEmpty(winner))
            {
                await Clients.Group(gameId).SendAsync("GameOver", winner);
                Games.TryRemove(gameId, out _);
                return;
            }

            game.CurrentPlayer = game.Players.First(p => p != game.CurrentPlayer);
        }


        public IEnumerable<string> GetRooms(string containsPlayer)
        {
            var res = Games.Keys;
            if(containsPlayer != "")
            {
                res = (ICollection<string>)res.Where(game => Games[game].Players.Contains(containsPlayer));
            }
            return res;
        }
    }
}
