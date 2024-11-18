using Microsoft.AspNetCore.Mvc.Rendering;

namespace Zad4.Models
{
    public class User
    {
        public string Username { get; set; }
        public string Password { get; set; }
        public bool IsActive { get; set; }
        public string Gender { get; set; }
        public string Bio { get; set; }
        public string Country { get; set; }
        public List<SelectListItem> CountryList { get; set; }

    }
}
