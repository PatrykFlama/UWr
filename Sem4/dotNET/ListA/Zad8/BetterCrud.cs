using Declarations;
using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Dapper;
using Declarations;


namespace Zad8
{
    internal class BetterSimplecrud
    {
        const string connectionString = @"data source=LAPTOP-GLATSD9;initial catalog=master;database=dotnetCourse;trusted_connection=true";

        static void MainStart()
        {
            RegisterStudent("Sname3", "Ssname", new DateTime(2024, 1, 1), "SSStreeeet", 1, 2, "12-345", "Neverland");
        }

        static public void RegisterStudent(string name, string surname, DateTime birthDate, string road, int houseNumber, int apartmentNumber, string postalCode, string localityName)
        {
            try
            {
                using (var connection = new SqlConnection(connectionString))
                {
                    connection.Open();
                    if (StudentExists(connection, name, surname, birthDate))
                    {
                        Console.WriteLine("student already in DB");
                        return;
                    }

                    long localityID = GetLocality(connection, localityName);
                    long addressID = GetAddress(connection, road, houseNumber, apartmentNumber, postalCode, localityID);
                    long studentID = AddStudent(connection, name, surname, birthDate);
                    BindStudentToAddress(connection, studentID, addressID);

                    Console.WriteLine("student added to DB");
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
            }
        }

        static private bool StudentExists(SqlConnection connection, string name, string surname, DateTime birthDate)
        {
            var student = connection.GetList<Student>(
                new { Name = name, Surname = surname, BirthDate = birthDate }
            );
            return (bool)student.Any();
        }

        static private long GetLocality(SqlConnection connection, string localityName)
        {
            var locality = connection.GetList<Locality>(
                new { Name = localityName });

            if (locality.Any())
            {
                return locality.First().ID;
            }
            else
            {
                var newLocality = new Locality { Name = localityName };
                connection.Insert(newLocality);
                return newLocality.ID;
            }
        }

        static private long GetAddress(SqlConnection connection, string road, int houseNumber, int apartmentNumber, string postalCode, long localityID)
        {
            var address = connection.GetList<Address>(
                new { Road = road, HouseNumber = houseNumber, ApartmentNumber = apartmentNumber, PostalCode = postalCode, LocalityID = localityID }
            );


            if (address.Any())
            {
                return address.First().ID;
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
                connection.Insert(newAddress);
                return newAddress.ID;
            }
        }

        static private long AddStudent(SqlConnection connection, string name, string surname, DateTime birthDate)
        {
            var newStudent = new Student
            {
                Name = name,
                Surname = surname,
                BirthDate = birthDate
            };
            connection.Insert(newStudent);
            return newStudent.ID;
        }

        static private void BindStudentToAddress(SqlConnection connection, long studentID, long addressID)
        {
            var studentAddress = new StudentAddress
            {
                StudentID = studentID,
                AddressID = addressID
            };
            connection.Insert(studentAddress);
        }
    }
}
