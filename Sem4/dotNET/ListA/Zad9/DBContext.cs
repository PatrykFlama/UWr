using Microsoft.EntityFrameworkCore;

namespace DBContextNS
{
    public class Student
    {
        public long ID { get; set; }
        public string Name { get; set; }
        public string Surname { get; set; }
        public DateTime BirthDate { get; set; }

        public virtual ICollection<Address> Addresses { get; set; }
    }

    public class Address
    {
        public long ID { get; set; }
        public string Road { get; set; }
        public int HouseNumber { get; set; }
        public int ApartmentNumber { get; set; }
        public string PostalCode { get; set; }
        public long LocalityID { get; set; }

        public virtual ICollection<Locality> Localities { get; set; }
    }

    public class Locality
    {
        public long ID { get; set; }
        public string Name { get; set; }
    }

    public class StudentAddress
    {
        public long ID { get; set; }
        public long StudentID { get; set; }
        public long AddressID { get; set; }

        public virtual ICollection<Student> Students { get; set; }
        public virtual ICollection<Student> Addresses { get; set; }

    }

    public class DotnetCourseDataContext : DbContext
    {
        public DotnetCourseDataContext() { }
        public DotnetCourseDataContext(string connectionString) : base(@"data source=LAPTOP-GLATSD9;initial catalog=master;database=dotnetCourseEF;trusted_connection=true") { }


        public DbSet<Student> Students { get; set; }
        public DbSet<Address> Addresses { get; set; }
        public DbSet<Locality> Localities { get; set; }
        public DbSet<StudentAddress> StudentAddresses { get; set; }
    }
}
