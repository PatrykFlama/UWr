using System.ComponentModel.DataAnnotations;
using System.Text.RegularExpressions;

namespace Zad5.CustomValidators
{
    public class LatinTextAttribute : ValidationAttribute
    {
        private static readonly Regex _regex = new Regex(@"^[a-zA-ZąćęłńóśźżĄĆĘŁŃÓŚŹŻ0-9\s]*$", RegexOptions.Compiled);

        protected override ValidationResult IsValid(object value, ValidationContext validationContext)
        {
            if(value == null || !(value is string text) || !_regex.IsMatch(text))
            {
                return new ValidationResult("Pole może zawierać tylko litery łacińskie, polskie znaki, cyfry i białe znaki.");
            }

            return ValidationResult.Success;
        }
    }

}
