using System;
using System.Data;
using System.Data.SqlClient;
using Dapper;
using System.Linq;

class Program
{
    const string connectionString = @"data source=LAPTOP-GLATSD9;initial catalog=master;database=dotnetCourse;trusted_connection=true";

    static void Main()
    {
        RegisterStudent("Sname2", "Ssname2", new DateTime(2024, 1, 1), "SSStreeeet2", 1, 2, "12-345", "Neverland");
    }

    static public void RegisterStudent(string name, string surname, DateTime birthDate, string road, int houseNumber, int apartmentNumber, string postalCode, string localityName)
    {
        try
        {
            using (SqlConnection connection = new SqlConnection(connectionString))
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
        string query = "SELECT COUNT(*) FROM Student WHERE Name = @Name AND Surname = @Surname AND BirthDate = @BirthDate";
        var count = connection.QuerySingle<int>(query, new { Name = name, Surname = surname, BirthDate = birthDate }, transaction: transaction);
        return count > 0;
    }

    static private long GetLocality(SqlConnection connection, SqlTransaction transaction, string localityName)
    {
        const string selectQuery = "SELECT ID FROM Locality WHERE Name = @LocalityName";
        var result = connection.QuerySingleOrDefault<long?>(selectQuery, new { LocalityName = localityName }, transaction: transaction);

        if (result.HasValue)
        {
            return result.Value;
        }
        else
        {
            const string insertQuery = "INSERT INTO Locality (Name) OUTPUT INSERTED.ID VALUES (@LocalityName)";
            return connection.QuerySingle<long>(insertQuery, new { LocalityName = localityName }, transaction: transaction);
        }
    }

    static private long GetAddress(SqlConnection connection, SqlTransaction transaction, string road, int houseNumber, int apartmentNumber, string postalCode, long localityID)
    {
        const string selectQuery = "SELECT ID FROM Address WHERE Road = @Road AND HouseNumber = @HouseNumber AND ApartmentNumber = @ApartmentNumber AND PostalCode = @PostalCode AND LocalityID = @LocalityID";
        var addressID = connection.QuerySingleOrDefault<long?>(selectQuery, new { Road = road, HouseNumber = houseNumber, ApartmentNumber = apartmentNumber, PostalCode = postalCode, LocalityID = localityID }, transaction: transaction);

        if (addressID.HasValue)
        {
            return addressID.Value;
        }
        else
        {
            const string insertQuery = "INSERT INTO Address (Road, HouseNumber, ApartmentNumber, PostalCode, LocalityID) OUTPUT INSERTED.ID VALUES (@Road, @HouseNumber, @ApartmentNumber, @PostalCode, @LocalityID)";
            return connection.QuerySingle<long>(insertQuery, new { Road = road, HouseNumber = houseNumber, ApartmentNumber = apartmentNumber, PostalCode = postalCode, LocalityID = localityID }, transaction: transaction);
        }
    }

    static private long AddStudent(SqlConnection connection, SqlTransaction transaction, string name, string surname, DateTime birthDate)
    {
        const string query = "INSERT INTO Student (Name, Surname, BirthDate) OUTPUT INSERTED.ID VALUES (@Name, @Surname, @BirthDate)";
        return connection.QuerySingle<long>(query, new { Name = name, Surname = surname, BirthDate = birthDate }, transaction: transaction);
    }

    static private void BindStudentToAddress(SqlConnection connection, SqlTransaction transaction, long studentID, long addressID)
    {
        const string query = "INSERT INTO StudentAddress (StudentID, AddressID) VALUES (@StudentID, @AddressID)";
        connection.Execute(query, new { StudentID = studentID, AddressID = addressID }, transaction: transaction);
    }
}
