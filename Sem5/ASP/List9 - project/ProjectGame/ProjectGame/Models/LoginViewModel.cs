using System.ComponentModel.DataAnnotations;

namespace ProjectGame.Models
{
    public class LoginViewModel
    {
        [Required(ErrorMessage = "Username is required.")]
        [MinLength(3)]
        public string Name { get; set; }

        [Required(ErrorMessage = "Password is required.")]
        [DataType(DataType.Password)]
        [MinLength(5)]
        public string Password { get; set; }

        public bool Errors { get; set; } = false;
    }

}
