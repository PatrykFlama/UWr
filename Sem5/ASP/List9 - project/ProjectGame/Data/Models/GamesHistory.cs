using System;

namespace Data.Models
{
    public class GamesHistory
    {
        public int Id { get; set; }
        public int PlayerXId { get; set; }
        public int PlayerOId { get; set; }
        public int PlayerXScore { get; set; }
        public DateTime GameDate { get; set; }
    }
}
