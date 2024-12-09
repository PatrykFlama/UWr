namespace Zad2.Data
{
    public class GenerateData
    {
        public static async Task CreateRoles(AppDbContext context)
        {
            if(!context.Roles.Any())
            {
                var adminRole = new Role { RoleName = "Admin" };
                var userRole = new Role { RoleName = "User" };

                await context.Roles.AddRangeAsync(adminRole, userRole);
                await context.SaveChangesAsync();
            }
        }

    }
}
