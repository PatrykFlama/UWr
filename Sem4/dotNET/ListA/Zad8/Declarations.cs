using System;
using Dapper;


namespace Declarations
{
    [Table("Student")]
    public class Student
    {
        [Key]
        public long ID { get; set; }
        public string Name { get; set; }
        public string Surname { get; set; }
        public DateTime BirthDate { get; set; }
    }

    [Table("Address")]
    public class Address
    {
        [Key]
        public long ID { get; set; }
        public string Road { get; set; }
        public int HouseNumber { get; set; }
        public int ApartmentNumber { get; set; }
        public string PostalCode { get; set; }
        public long LocalityID { get; set; }
    }

    [Table("Locality")]
    public class Locality
    {
        [Key]
        public long ID { get; set; }
        public string Name { get; set; }
    }

    [Table("StudentAddress")]
    public class StudentAddress
    {
        [Key]
        public long ID { get; set; }
        public long StudentID { get; set; }
        public long AddressID { get; set; }
    }


}