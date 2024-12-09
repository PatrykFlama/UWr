using Zad2.Data;
using Microsoft.EntityFrameworkCore;

namespace Zad2.Helpers
{
    public class AuthService
    {
        private readonly AppDbContext _context;
        private readonly PasswordHasher _hasher = new();

        public AuthService(AppDbContext context)
        {
            _context = context;
        }

        public async Task Register(string userName, string email, string password, string role)
        {
            var hashedPassword = _hasher.HashPassword(password);

            var user = new User { UserName = userName, Email = email };
            await _context.Users.AddAsync(user);
            await _context.SaveChangesAsync();

            var passwordEntity = new Password
            {
                UserId = user.Id,
                PasswordHash = hashedPassword
            };
            await _context.Passwords.AddAsync(passwordEntity);

            var selectedRole = await _context.Roles.SingleOrDefaultAsync(r => r.RoleName == role);
            if(selectedRole == null)
            {
                throw new Exception("Wybrana rola nie istnieje.");
            }

            var userRole = new UserRole
            {
                UserId = user.Id,
                RoleId = selectedRole.Id
            };
            await _context.UserRoles.AddAsync(userRole);

            await _context.SaveChangesAsync();
        }


        public async Task<User> Login(string email, string password)
        {
            var user = await _context.Users.SingleOrDefaultAsync(u => u.Email == email);
            if(user == null) return null;

            var passwordEntity = await _context.Passwords.SingleOrDefaultAsync(p => p.UserId == user.Id);
            if(passwordEntity == null) return null;

            if(_hasher.VerifyPassword(password, passwordEntity.PasswordHash))
            {
                return user;
            }

            return null;
        }

        public async Task<string> GetUserRole(int userId)
        {
            var userRole = await _context.UserRoles
                                        .Where(ur => ur.UserId == userId)
                                        .Select(ur => ur.Role.RoleName)
                                        .FirstOrDefaultAsync();

            return userRole;
        }
    }
}