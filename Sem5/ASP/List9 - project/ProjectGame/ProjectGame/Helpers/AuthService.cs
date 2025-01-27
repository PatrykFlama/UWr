using Data;
using Microsoft.EntityFrameworkCore;
using Data.Models;
using Microsoft.AspNetCore.Identity;

namespace ProjectGame.Helpers
{
    public class AuthService
    {
        private readonly AppDbContext _context;
        private readonly PasswordHasher _hasher = new();

        public AuthService(AppDbContext context)
        {
            _context = context;
        }

        public async Task<bool> Register(string name, string password)
        {
            var checkuser = await _context.Users.SingleOrDefaultAsync(u => u.Name == name);
            if(checkuser != null)
            {
                return false;
            }

            var hashedPassword = _hasher.HashPassword(password);

            var user = new User { Name = name };
            await _context.Users.AddAsync(user);
            await _context.SaveChangesAsync();

            var passwordEntity = new Password
            {
                UserId = user.Id,
                PasswordHash = hashedPassword
            };
            await _context.Passwords.AddAsync(passwordEntity);

            await _context.SaveChangesAsync();

            return true;
        }


        public async Task<User> Login(string name, string password)
        {
            var user = await _context.Users.SingleOrDefaultAsync(u => u.Name == name);
            if(user == null) return null;

            var passwordEntity = await _context.Passwords.SingleOrDefaultAsync(p => p.UserId == user.Id);
            if(passwordEntity == null) return null;

            if(_hasher.VerifyPassword(password, passwordEntity.PasswordHash))
            {
                return user;
            }

            return null;
        }
    }
}