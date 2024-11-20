using System.ComponentModel.DataAnnotations;

namespace Zad5.CustomValidators
{
    public class PeselAttribute : ValidationAttribute
    {
        protected override ValidationResult IsValid(object value, ValidationContext validationContext)
        {
            if(value == null || !(value is string pesel) || pesel.Length != 11 || !long.TryParse(pesel, out _))
            {
                return new ValidationResult("PESEL musi mieć 11 cyfr");
            }

            //// sprawdzenie liczby weryfikacyjnej
            //int[] weights = { 1, 3, 7, 9, 1, 3, 7, 9, 1, 3 };
            //int checksum = 0;
            //for(int i = 0; i < 10; i++)
            //{
            //    checksum += weights[i] * (pesel[i] - '0');
            //}

            //int controlDigit = (10 - (checksum % 10)) % 10;

            //if(controlDigit != (pesel[10] - '0'))
            //{
            //    return new ValidationResult("numer PESEL jest niepoprawny");
            //}

            return ValidationResult.Success;
        }
    }

}
