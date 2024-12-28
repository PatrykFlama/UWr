using System.ComponentModel.DataAnnotations;

namespace NetCoreSignalR.Models
{
    public class AccountLogonModel
    {
        [Required]
        public string UserName { get; set; }
        public string Password { get; set; }
    }
}
