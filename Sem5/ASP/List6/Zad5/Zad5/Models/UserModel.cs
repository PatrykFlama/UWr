using Zad5.CustomValidators;

namespace Zad5.Models
{
    public class UserModel
    {
        [Pesel]
        public string Pesel { get; set; }

        [LatinText]
        public string FullName { get; set; }
    }
}
