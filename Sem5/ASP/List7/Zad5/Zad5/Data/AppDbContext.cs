using Microsoft.EntityFrameworkCore;

namespace Zad5.Data
{
    public class AppDbContext : DbContext
    {
        public AppDbContext(DbContextOptions<AppDbContext> options) : base(options) { }

        public DbSet<UserTOTP> UsersTOTP { get; set; }
        public DbSet<UserTOTPKey> UserTOTPKeys { get; set; }

        //protected override void OnModelCreating(ModelBuilder modelBuilder)
        //{
        //    modelBuilder.Entity<UserTOTPKey>()
        //        .HasOne(u => u.User)
        //        .WithMany(u => u.TOTPKeys)
        //        .HasForeignKey(u => u.UserID);
        //}
    }
}
