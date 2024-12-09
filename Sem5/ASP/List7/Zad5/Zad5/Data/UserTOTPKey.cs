namespace Zad5.Data
{
    public class UserTOTPKey
    {
        public int ID { get; set; }
        public int UserTOTPID { get; set; }
        public string TOTPKey { get; set; }
        public DateTime CreatedAt { get; set; }

        //public UserTOTP User { get; set; }
    }

}
