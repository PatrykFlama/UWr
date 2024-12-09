namespace Zad5.Data
{
    public class UserTOTP
    {
        public int Id { get; set; }
        public string UserName { get; set; }
        public ICollection<UserTOTPKey> TOTPKeys { get; set; }
    }
}
