using System;
using System.Data;
using System.Data.SqlClient;
using System.Threading.Tasks.Dataflow;
using System.Transactions;


class Program
{
    const string connectionString = @"data source=LAPTOP-GLATSD9;initial catalog=master;database=dotnetCourse;trusted_connection=true";

    static void Main()
    {
        RegisterStudent("Sname", "Ssname", new DateTime(2024, 1, 1), "SSStreeeet", 1, 2, "12-345", "Neverland");

        //try
        //{
        //    using (SqlConnection conn = new SqlConnection(connectionString))
        //    {
        //        conn.Open();
        //        Console.WriteLine("Connected to database");
        //    }
        //} catch (Exception e) {
        //    Console.WriteLine(e.Message);
        //}
    }




    static public void RegisterStudent(string name, string surname, DateTime birthDate, string road, int houseNumber, int apartmentNumber, string postalCode, string localityName)
    {
        try
        {
            using (SqlConnection connection = new SqlConnection(connectionString))
            {
                connection.Open();
                using (SqlTransaction transaction = connection.BeginTransaction())
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
                    Console.WriteLine("sudent added to DB");
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
        using (SqlCommand command = new SqlCommand(query, connection, transaction))
        {
            command.Parameters.AddWithValue("@Name", name);
            command.Parameters.AddWithValue("@Surname", surname);
            command.Parameters.AddWithValue("@BirthDate", birthDate);
            int count = (int)command.ExecuteScalar();
            return count > 0;
        }
    }

    // get locality id (if it does not exists, create one)
    static private long GetLocality(SqlConnection connection, SqlTransaction transaction, string localityName)
    {
        const string selectQuery = "SELECT ID FROM Locality WHERE Name = @LocalityName";
        using (SqlCommand command = new SqlCommand(selectQuery, connection, transaction))
        {
            command.Parameters.AddWithValue("@LocalityName", localityName);
            object result = command.ExecuteScalar();

            if (result != null)
            {
                return (long)result;
            }
            else
            {
                string insertQuery = "INSERT INTO Locality (Name) OUTPUT INSERTED.ID VALUES (@LocalityName)";
                using (SqlCommand insertCommand = new SqlCommand(insertQuery, connection, transaction))
                {
                    insertCommand.Parameters.AddWithValue("@LocalityName", localityName);
                    return (long)insertCommand.ExecuteScalar();
                }
            }
        }
    }

   // get address id (or create one if it does not exist)
    static private long GetAddress(SqlConnection connection, SqlTransaction transaction, string road, int houseNumber, int apartmentNumber, string postalCode, long localityID)
    {
        const string selectQuery = "SELECT ID FROM Address WHERE Road = @Road AND HouseNumber = @HouseNumber AND ApartmentNumber = @ApartmentNumber AND PostalCode = @PostalCode AND LocalityID = @LocalityID";
        using (SqlCommand selectCommand = new SqlCommand(selectQuery, connection, transaction))
        {
            selectCommand.Parameters.AddWithValue("@Road", road);
            selectCommand.Parameters.AddWithValue("@HouseNumber", houseNumber);
            selectCommand.Parameters.AddWithValue("@ApartmentNumber", apartmentNumber);
            selectCommand.Parameters.AddWithValue("@PostalCode", postalCode);
            selectCommand.Parameters.AddWithValue("@LocalityID", localityID);
            object addressID = selectCommand.ExecuteScalar();

            if (addressID != null)
            {
                return (long)addressID;
            }
            else
            {
                const string insertQuery = "INSERT INTO Address (Road, HouseNumber, ApartmentNumber, PostalCode, LocalityID) OUTPUT INSERTED.ID VALUES (@Road, @HouseNumber, @ApartmentNumber, @PostalCode, @LocalityID)";
                using (SqlCommand insertCommand = new SqlCommand(insertQuery, connection, transaction))
                {
                    insertCommand.Parameters.AddWithValue("@Road", road);
                    insertCommand.Parameters.AddWithValue("@HouseNumber", houseNumber);
                    insertCommand.Parameters.AddWithValue("@ApartmentNumber", apartmentNumber);
                    insertCommand.Parameters.AddWithValue("@PostalCode", postalCode);
                    insertCommand.Parameters.AddWithValue("@LocalityID", localityID);
                    return (long)insertCommand.ExecuteScalar();
                }
            }
        }
    }


    static private long AddStudent(SqlConnection connection, SqlTransaction transaction, string name, string surname, DateTime birthDate)
    {
        const string query = "INSERT INTO Student (Name, Surname, BirthDate) OUTPUT INSERTED.ID VALUES (@Name, @Surname, @BirthDate)";
        using (SqlCommand command = new SqlCommand(query, connection, transaction))
        {
            command.Parameters.AddWithValue("@Name", name);
            command.Parameters.AddWithValue("@Surname", surname);
            command.Parameters.AddWithValue("@BirthDate", birthDate);
            return (long)command.ExecuteScalar();
        }
    }


    static private void BindStudentToAddress(SqlConnection connection, SqlTransaction transaction, long studentID, long addressID)
    {
        const string query = "INSERT INTO StudentAddress (StudentID, AddressID) VALUES (@StudentID, @AddressID)";
        using (SqlCommand command = new SqlCommand(query, connection, transaction))
        {
            command.Parameters.AddWithValue("@StudentID", studentID);
            command.Parameters.AddWithValue("@AddressID", addressID);
            command.ExecuteNonQuery();
        }
    }

}

