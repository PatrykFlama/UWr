using System;
using System.Data.SqlClient;
using Dapper;
using Declarations;

class Program
{
    const string connectionString = @"data source=LAPTOP-GLATSD9;initial catalog=master;database=dotnetCourse;trusted_connection=true";

    static void Main()
    {
        RegisterStudent("Sname", "Ssname", new DateTime(2024, 1, 1), "SSStreeeet", 1, 2, "12-345", "Neverland");
    }

    static public void RegisterStudent(string name, string surname, DateTime birthDate, string road, int houseNumber, int apartmentNumber, string postalCode, string localityName)
    {
        try
        {
            using (var connection = new SqlConnection(connectionString))
            {
                connection.Open();
                using (var transaction = connection.BeginTransaction())
                {
                    if (StudentExists(connection, transaction, name, surname, birthDate))
                    {
                        Console.WriteLine("student already in DB");
                        return;
                    }

                    long localityID = GetLocality(connection, transaction, localityName);
                    long addressID = GetAddress(connection, transaction, road, houseNumber, apartmentNumber, postalCode, localityID);
                    long studentID = AddStudent(connection, transaction, name, surname, birthDate);
                    BindStudentToAddress(connection, transaction, studentID, addressID);

                    transaction.Commit();
                    Console.WriteLine("student added to DB");
                }
            }
        }
        catch (Exception e)
        {
            Console.WriteLine(e.Message);
        }
    }

    static private bool StudentExists(SqlConnection connection, SqlTransaction transaction, string name, string surname, DateTime birthDate)
    {
        var student = connection.QueryFirstOrDefault<Student>(
            "SELECT * FROM Student WHERE Name = @Name AND Surname = @Surname AND BirthDate = @BirthDate",
            new { Name = name, Surname = surname, BirthDate = birthDate },
            transaction
        );
        return student != null;
    }

    static private long GetLocality(SqlConnection connection, SqlTransaction transaction, string localityName)
    {
        var locality = connection.QueryFirstOrDefault<Locality>("SELECT * FROM Locality WHERE Name = @LocalityName", new { LocalityName = localityName }, transaction);

        if (locality != null)
        {
            return locality.ID;
        }
        else
        {
            var newLocality = new Locality { Name = localityName };
            connection.Insert(newLocality, transaction);
            return newLocality.ID;
        }
    }

    static private long GetAddress(SqlConnection connection, SqlTransaction transaction, string road, int houseNumber, int apartmentNumber, string postalCode, long localityID)
    {
        var address = connection.QueryFirstOrDefault<Address>(
            "SELECT * FROM Address WHERE Road = @Road AND HouseNumber = @HouseNumber AND ApartmentNumber = @ApartmentNumber AND PostalCode = @PostalCode AND LocalityID = @LocalityID",
            new { Road = road, HouseNumber = houseNumber, ApartmentNumber = apartmentNumber, PostalCode = postalCode, LocalityID = localityID },
            transaction
        );

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
            connection.Insert(newAddress, transaction);
            return newAddress.ID;
        }
    }

    static private long AddStudent(SqlConnection connection, SqlTransaction transaction, string name, string surname, DateTime birthDate)
    {
        var newStudent = new Student
        {
            Name = name,
            Surname = surname,
            BirthDate = birthDate
        };
        connection.Insert(newStudent, transaction);
        return newStudent.ID;
    }

    static private void BindStudentToAddress(SqlConnection connection, SqlTransaction transaction, long studentID, long addressID)
    {
        var studentAddress = new StudentAddress
        {
            StudentID = studentID,
            AddressID = addressID
        };
        connection.Insert(studentAddress, transaction);
    }
}
