using System.ComponentModel.DataAnnotations;

namespace MediatRDemo.Models.Account
{
    public class LogonModel
    {
        [Required]
        public string Username { get; set; }
        [Required]
        public string Password { get; set; }
    }
}
