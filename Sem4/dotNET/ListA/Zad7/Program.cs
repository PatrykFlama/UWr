using System;
using System.Linq;
using System.Net;

class Program
{
    //const string connectionString = @"data source=LAPTOP-GLATSD9;initial catalog=master;database=dotnetCourse;trusted_connection=true";

    //static void Main()
    //{
    //    RegisterStudent("Sname3", "Ssname3", new DateTime(2024, 1, 1), "SSStreeeet", 1, 2, "12-345", "Neverland");
    //}

    //static public void RegisterStudent(string name, string surname, DateTime birthDate, string road, int houseNumber, int apartmentNumber, string postalCode, string localityName)
    //{
    //    try
    //    {
    //        using (var context = new DotnetCourseDataContext(connectionString))
    //        {
    //            using (var transaction = context.Connection.BeginTransaction())
    //            {
    //                context.Transaction = transaction;

    //                if (StudentExists(context, name, surname, birthDate))
    //                {
    //                    Console.WriteLine("student already in DB");
    //                    return;
    //                }

    //                long localityID = GetLocality(context, localityName);
    //                long addressID = GetAddress(context, road, houseNumber, apartmentNumber, postalCode, localityID);
    //                long studentID = AddStudent(context, name, surname, birthDate);
    //                BindStudentToAddress(context, studentID, addressID);

    //                context.SubmitChanges();
    //                transaction.Commit();
    //                Console.WriteLine("student added to DB");
    //            }
    //        }
    //    }
    //    catch (Exception e)
    //    {
    //        Console.WriteLine(e.Message);
    //    }
    //}

    //static private bool StudentExists(DotnetCourseDataContext context, string name, string surname, DateTime birthDate)
    //{
    //    return context.Students.Any(s => s.Name == name && s.Surname == surname && s.BirthDate == birthDate);
    //}

    //static private long GetLocality(DotnetCourseDataContext context, string localityName)
    //{
    //    var locality = context.Localities.FirstOrDefault(l => l.Name == localityName);

    //    if (locality != null)
    //    {
    //        return locality.ID;
    //    }
    //    else
    //    {
    //        var newLocality = new Locality { Name = localityName };
    //        context.Localities.InsertOnSubmit(newLocality);
    //        context.SubmitChanges();
    //        return newLocality.ID;
    //    }
    //}

    //static private long GetAddress(DotnetCourseDataContext context, string road, int houseNumber, int apartmentNumber, string postalCode, long localityID)
    //{
    //    var address = context.Addresses.FirstOrDefault(a => a.Road == road && a.HouseNumber == houseNumber && a.ApartmentNumber == apartmentNumber && a.PostalCode == postalCode && a.LocalityID == localityID);

    //    if (address != null)
    //    {
    //        return address.ID;
    //    }
    //    else
    //    {
    //        var newAddress = new Address
    //        {
    //            Road = road,
    //            HouseNumber = houseNumber,
    //            ApartmentNumber = apartmentNumber,
    //            PostalCode = postalCode,
    //            LocalityID = localityID
    //        };
    //        context.Addresses.InsertOnSubmit(newAddress);
    //        context.SubmitChanges();
    //        return newAddress.ID;
    //    }
    //}

    //static private long AddStudent(DotnetCourseDataContext context, string name, string surname, DateTime birthDate)
    //{
    //    var newStudent = new Student
    //    {
    //        Name = name,
    //        Surname = surname,
    //        BirthDate = birthDate
    //    };
    //    context.Students.InsertOnSubmit(newStudent);
    //    context.SubmitChanges();
    //    return newStudent.ID;
    //}

    //static private void BindStudentToAddress(DotnetCourseDataContext context, long studentID, long addressID)
    //{
    //    var studentAddress = new StudentAddress
    //    {
    //        StudentID = studentID,
    //        AddressID = addressID
    //    };
    //    context.StudentAddresses.InsertOnSubmit(studentAddress);
    //    context.SubmitChanges();
    //}
}
