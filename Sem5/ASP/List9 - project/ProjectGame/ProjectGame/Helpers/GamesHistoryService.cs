using Data;
using Data.Models;
using Microsoft.EntityFrameworkCore;

namespace ProjectGame.Helpers
{
    public class GamesHistoryService
    {
        private readonly AppDbContext _context;

        public GamesHistoryService(AppDbContext context)
        {
            _context = context;
        }


        public async Task AddGame(string playerXName, string playerOName, int playerXScore)
        {
            var playerXId = 0;
            var playerOId = 0;

            if(playerXName != "")
                playerXId = (await _context.Users.FirstOrDefaultAsync(u => u.Name == playerXName)).Id;
            if(playerOName != "")
                playerOId = (await _context.Users.FirstOrDefaultAsync(u => u.Name == playerOName)).Id;

            var game = new GamesHistory
            {
                PlayerXId = playerXId,
                PlayerOId = playerOId,
                PlayerXScore = playerXScore,
                GameDate = DateTime.Now,
            };

            await _context.GamesHistory.AddAsync(game);
            await _context.SaveChangesAsync();
        }


        public async Task<List<GamesHistory>> GetGamesByPlayer(int playerId)
        {
            return await _context.GamesHistory
                .Where(g => g.PlayerXId == playerId || g.PlayerOId == playerId)
                .ToListAsync();
        }


        public async Task<List<GamesHistory>> GetGamesWonByPlayer(int playerId)
        {
            return await _context.GamesHistory
                .Where(g => (g.PlayerXId == playerId && g.PlayerXScore > 0))
                .ToListAsync();
        }


        public async Task<List<GamesHistory>> GetRecentGames(int count)
        {
            return await _context.GamesHistory
                .OrderByDescending(g => g.GameDate)
                .Take(count)
                .ToListAsync();
        }


        public async Task<GamesHistory> GetGameById(int gameId)
        {
            return await _context.GamesHistory
                .FirstOrDefaultAsync(g => g.Id == gameId);
        }


        public async Task DeleteGame(int gameId)
        {
            var game = await _context.GamesHistory.FirstOrDefaultAsync(g => g.Id == gameId);
            if (game != null)
            {
                _context.GamesHistory.Remove(game);
                await _context.SaveChangesAsync();
            }
        }


        public async Task<List<GamesHistory>> GetAllGames()
        {
            return await _context.GamesHistory.ToListAsync();
        }
    }
}
