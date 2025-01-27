using Data.Models;
using System.ComponentModel.DataAnnotations;

namespace ProjectGame.Models
{
    public class GamesHistoryViewModel
    {
        public List<GamesHistory> history { get; set; } = new List<GamesHistory>();
    }

}
