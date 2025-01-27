using Microsoft.AspNetCore.SignalR;
using System.Collections.Concurrent;
using GameLogic;
using Microsoft.AspNetCore.Authorization;
using ProjectGame.Helpers;
using Data;
using Microsoft.EntityFrameworkCore;

namespace ProjectGame.Hubs
{
    public class GameHub : Hub
    {
        private static readonly ConcurrentDictionary<string, TicTacToeGame> Games = new();
        AppDbContext _context;
        GamesHistoryService _historyService;

        public GameHub(AppDbContext context, GamesHistoryService historyService)
        {
            _context = context;
            _historyService = historyService;
        }

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

            var playerIdentifier = Context.ConnectionId;
            if(!game.Players.Contains(playerIdentifier))
            {
                game.Players.Add(playerIdentifier);
                game.PlayerNames.Add((Context.User.Identity.IsAuthenticated ? 
                    Context.User.Identity.Name : ""));
            }

            if(game.Players.Count == 2)
            {
                game.CurrentPlayer = game.Players[0];
                game.CurrentPlayerRole = game.PlayerSymbols[0];
            }

            await Groups.AddToGroupAsync(playerIdentifier, gameId);
            await Clients.Group(gameId).SendAsync("PlayerJoined", game.Players);
            await Clients.Group(gameId).SendAsync("GameState", game.Board, game.CurrentPlayerRole);

            await Clients.Client(game.Players[0]).SendAsync("PlayerRole", game.PlayerSymbols[0]);
            await Clients.Client(game.Players[1]).SendAsync("PlayerRole", game.PlayerSymbols[1]);
        }

        public async Task MakeMove(string gameId, string srow, string scol)
        {
            int row = int.Parse(srow), col = int.Parse(scol); //! TODO why?

            if(!Games.TryGetValue(gameId, out var game) || game.Players.Count < 2)
            {
                throw new HubException("Invalid game state");
            }

            var playerIdentifier = Context.ConnectionId;
            if(game.CurrentPlayer != playerIdentifier)
            {
                throw new HubException("Not your turn");
            }

            if(game.Board[row][col] != ' ')
            {
                throw new HubException("Cell already occupied");
            }


            var symbol = game.Players[0] == playerIdentifier ? game.PlayerSymbols[0] : game.PlayerSymbols[1];
            game.Board[row][col] = symbol;
            game.CurrentPlayerRole = game.CurrentPlayerRole == 'X' ? 'O' : 'X';
            await Clients.Group(gameId).SendAsync("MoveMade", row, col, symbol, game.CurrentPlayerRole);

            char? winner = game.CheckWinner();
            if(winner != null)
            {
                await Clients.Group(gameId).SendAsync("GameOver", winner);

                string playerXname = game.PlayerNames[0];
                string playerYname = game.PlayerNames[1];
                int Xscore = (winner == null ? 0 : (winner == 'X' ? 1 : -1));
                await _historyService.AddGame(playerXname, playerYname, Xscore);
                
                Games.TryRemove(gameId, out _);
                return;
            }

            game.CurrentPlayer = game.Players.First(p => p != game.CurrentPlayer);
        }

        public override async Task OnDisconnectedAsync(Exception? exception)
        {
            var playerIdentifier = Context.ConnectionId;
            var gameId = Games.Keys
                .First(gameId => Games[gameId].Players.Contains(playerIdentifier));

            if(!string.IsNullOrEmpty(gameId))
            {
                if(Games[gameId].Players.Count == 2 && 
                   Games[gameId].Players[0] == playerIdentifier)
                {
                    Games[gameId].PlayerSymbols[0] = (Games[gameId].PlayerSymbols[0] == 'O' ? 'X' : 'O');
                    Games[gameId].PlayerSymbols[1] = (Games[gameId].PlayerSymbols[1] == 'O' ? 'X' : 'O');
                }

                Games[gameId].Players.Remove(playerIdentifier);

                if(Games[gameId].Players.Count == 0)
                {
                    Games.Remove(gameId, out var trash);
                }
            }
        }


        public async Task<IEnumerable<string>> GetRooms(string containsPlayer)
        {
            var res = Games.Keys;
            if(containsPlayer != "")
            {
                res = (ICollection<string>)res
                    .Where(game => Games[game].Players.Contains(containsPlayer));
            }
            return res;
        }
    }
}
