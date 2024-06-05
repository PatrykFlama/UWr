using DBContextNS;
using System;
using System.Linq;
using DBContextNS;
using Microsoft.EntityFrameworkCore;




class Program
{
    static void Main()
    {
        RegisterStudent("Sname", "Ssname", new DateTime(2024, 1, 1), "SSStreeeet", 1, 2, "12-345", "Neverland");
    }

    static public void RegisterStudent(string name, string surname, DateTime birthDate, string road, int houseNumber, int apartmentNumber, string postalCode, string localityName)
    {
        try
        {
            var builder = new DbContextOptionsBuilder<DotnetCourseDataContext>();

            builder.UseSqlServer(@"data source=LAPTOP-GLATSD9;initial catalog=master;database=dotnetCourseEF;trusted_connection=true");

            using (var context = new DotnetCourseDataContext(builder.Options))
            {
                var existingStudent = context.Students.FirstOrDefault(s => s.Name == name && s.Surname == surname && s.BirthDate == birthDate);
                if (existingStudent != null)
                {
                    Console.WriteLine("student already in DB");
                    return;
                }

                long localityID = GetOrCreateLocalityID(context, localityName);
                long addressID = GetOrCreateAddressID(context, road, houseNumber, apartmentNumber, postalCode, localityID);
                long studentID = AddStudent(context, name, surname, birthDate);
                BindStudentToAddress(context, studentID, addressID);

                context.SaveChanges();
                Console.WriteLine("student added to DB");
            }
        }
        catch (Exception e)
        {
            Console.WriteLine(e.Message);
        }
    }

    static private long GetOrCreateLocalityID(DotnetCourseDataContext context, string localityName)
    {
        var locality = context.Localities.FirstOrDefault(l => l.Name == localityName);
        if (locality != null)
        {
            return locality.ID;
        }
        else
        {
            var newLocality = new Locality { Name = localityName };
            context.Localities.Add(newLocality);
            context.SaveChanges();
            return newLocality.ID;
        }
    }

    static private long GetOrCreateAddressID(DotnetCourseDataContext context, string road, int houseNumber, int apartmentNumber, string postalCode, long localityID)
    {
        var address = context.Addresses.FirstOrDefault(a => a.Road == road && a.HouseNumber == houseNumber && a.ApartmentNumber == apartmentNumber && a.PostalCode == postalCode && a.LocalityID == localityID);
        if (address != null)
        {
            return address.ID;
        }
        else
        {
            var newAddress = new Address
            {
                Road = road,
                HouseNumber = houseNumber,
                ApartmentNumber = apartmentNumber,
                PostalCode = postalCode,
                LocalityID = localityID
            };
            context.Addresses.Add(newAddress);
            context.SaveChanges();
            return newAddress.ID;
        }
    }

    static private long AddStudent(DotnetCourseDataContext context, string name, string surname, DateTime birthDate)
    {
        var newStudent = new Student
        {
            Name = name,
            Surname = surname,
            BirthDate = birthDate
        };
        context.Students.Add(newStudent);
        context.SaveChanges();
        return newStudent.ID;
    }

    static private void BindStudentToAddress(DotnetCourseDataContext context, long studentID, long addressID)
    {
        var studentAddress = new StudentAddress
        {
            StudentID = studentID,
            AddressID = addressID
        };
        context.StudentAddresses.Add(studentAddress);
        context.SaveChanges();
    }
}
