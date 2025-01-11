using Microsoft.EntityFrameworkCore;
using System.Collections.Generic;
using Data.Models;

namespace Data
{
    public class AppDbContext : DbContext
    {
        public AppDbContext(DbContextOptions<AppDbContext> options) : base(options) { }

        public DbSet<User> Users { get; set; }
        public DbSet<Password> Passwords { get; set; }
        public DbSet<GamesHistory> GamesHistory { get; set; }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);

            modelBuilder.Entity<User>()
                .HasKey(u => u.Id);

            modelBuilder.Entity<Password>()
                .HasKey(p => p.Id);

            modelBuilder.Entity<GamesHistory>()
                .HasKey(gh => gh.Id);


            // Configure relationships for GamesHistory
            modelBuilder.Entity<GamesHistory>()
                .HasOne<User>()
                .WithMany()
                .HasForeignKey(gh => gh.PlayerXId)
                .OnDelete(DeleteBehavior.NoAction);

            modelBuilder.Entity<GamesHistory>()
                .HasOne<User>()
                .WithMany()
                .HasForeignKey(gh => gh.PlayerOId)
                .OnDelete(DeleteBehavior.NoAction);
        }
    }
}
