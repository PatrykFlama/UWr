using Microsoft.AspNetCore.SignalR;
using System.Collections.Concurrent;
using GameLogic;

namespace ProjectGame.Hubs
{
    public class GameHub : Hub
    {
        private static readonly ConcurrentDictionary<string, GameState> Games = new();

        public async Task JoinGame(string gameId)
        {
            if(!Games.ContainsKey(gameId))
            {
                // Create a new game if it doesn't exist
                Games[gameId] = new GameState
                {
                    GameId = gameId,
                    Players = new List<string>(),
                    Board = new int[3, 3], // Example board size
                    Scores = new Dictionary<string, int>()
                };
            }

            var game = Games[gameId];
            if(game.Players.Count >= 2)
            {
                throw new HubException("This game room is full.");
            }

            if(!game.Players.Contains(Context.ConnectionId))
            {
                game.Players.Add(Context.ConnectionId);
            }

            await Groups.AddToGroupAsync(Context.ConnectionId, gameId);
            await Clients.Group(gameId).SendAsync("PlayerJoined", Context.ConnectionId, game.Players);

            // Send initial game state to the joining player
            await Clients.Caller.SendAsync("GameState", game.Board, game.Scores, game.Players);
        }

        public async Task MakeMove(bool isHorizontal, int row, int col)
        {
            var gameId = GetGameIdFromConnection(Context.ConnectionId);
            if(string.IsNullOrEmpty(gameId) || !Games.ContainsKey(gameId))
            {
                throw new HubException("Game not found.");
            }

            var game = Games[gameId];
            // Validate move
            if(!IsValidMove(game, isHorizontal, row, col))
            {
                throw new HubException("Invalid move.");
            }

            // Apply move
            var currentPlayer = Context.ConnectionId;
            ApplyMove(game, isHorizontal, row, col, currentPlayer);

            // Broadcast updated state
            await Clients.Group(gameId).SendAsync("MoveMade", isHorizontal, row, col, currentPlayer, game.Board, game.Scores, GetNextPlayer(game));
        }

        private bool IsValidMove(GameState game, bool isHorizontal, int row, int col)
        {
            // Check if the move is valid (not already taken, within bounds, etc.)
            return true; // Replace with actual validation logic
        }

        private void ApplyMove(GameState game, bool isHorizontal, int row, int col, string currentPlayer)
        {
            // Update game state with the move
            // Update scores, mark the move, etc.
        }

        private string GetGameIdFromConnection(string connectionId)
        {
            // Find the game ID for the given connection ID
            foreach(var game in Games)
            {
                if(game.Value.Players.Contains(connectionId))
                {
                    return game.Key;
                }
            }
            return null;
        }

        private string GetNextPlayer(GameState game)
        {
            // Determine the next player's connection ID
            var currentIndex = game.Players.IndexOf(Context.ConnectionId);
            return game.Players[(currentIndex + 1) % game.Players.Count];
        }

        private class GameState
        {
            public string GameId { get; set; }
            public List<string> Players { get; set; }
            public int[,] Board { get; set; }
            public Dictionary<string, int> Scores { get; set; }
        }

    }
}
