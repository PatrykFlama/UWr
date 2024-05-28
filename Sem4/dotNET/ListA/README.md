

|1|2|3|4|5|6|7|8|9|10|
|-|-|-|-|-|-|-|-|-|--|
|x|x|x|x|x|X|X|X|X|~ |

* zad 7 problem z tworzeniem dbml
* EF problem ze stringiem łączącym do bazy danych (inicjalizacja)
* w dapper simple crud jak korzystać z tranzakcji
* dapper simplecrud problem z foreign key

## Zad1
Developer udostępnia wszystkie funkcjonalności z enterprize, ale licencja pozwala na wykorzystanie jej tylko do rozwoju oprogramowania
Express to darmowa werjsa bazy danych, okrojona z możliwości
LocalDB jest lżejsza dla systemu, bo baza danych jest włączana tylko wtedy kiedy jest potrzebna (przy ustalaniu połączenia)

## Zad2
```sql
drop table StudentAddress; 
GO
drop table Student; 
GO
drop table Address; 
GO
drop table Locality; 
GO

create table Student (
	ID bigint NOT NULL IDENTITY(1,1) PRIMARY KEY,
	Name varchar(50),
	Surname varchar(50),
	BirthDate date,
);
GO 


create table Locality (
	ID bigint not null IDENTITY(1,1) primary key,
	Name varchar(50),
);
GO

create table Address (
	ID bigint not null IDENTITY(1,1) primary key,
	Road varchar(50),
	HouseNumber int,
	ApartmentNumber int,
	PostalCode char(6),
	LocalityID bigint,
	foreign key (LocalityID) references Locality(ID),
);
GO

create table StudentAddress (
    StudentID bigint,
    AddressID bigint,
    primary key (StudentID, AddressID),
    foreign key (StudentID) REFERENCES Student(ID),
    foreign key (AddressID) references Address(ID)
);
GO


```


## Zad4
Problem pojawia się przy wielowątkowości, gdy to samo (lub podobne) zapytanie wywołamy w tym samym czasie, więc oba wywołania stwierdzą że rekordu brakuje w bazie danych i go dodadzą (podwójnie)  
Aby temu zapobiegać możemy zastosować interfejs serializable do naszej tranzakcji

```sql
using (var scope = new TransactionScope(TransactionScopeOption.Required, new TransactionOptions { IsolationLevel = IsolationLevel.Serializable }))
{
    -- // ...
    transaction.Commit();
}
```

dodatkowo można wprowadzać różne zabezpieczenia na poziomie bazy danych, np słowa kluczowe `unique`, które mogą zmniejszyć prawdopodobieństwo duplikatów (im więcej rekordów musi być unikatowych, tym ciężej dodać duplikat)

## Zad5
```sql
using (SqlConnection conn = new SqlConnection(connectionString))
{
    conn.Open();

    // rozpoczynamy tranzakcję - w tym momencie stawiamy punkt powrotu
    SqlTransaction transaction = conn.BeginTransaction();
    
    try
    {
        // operujemy na bazie danych

        // commit zatwierdza tranzakcję
        transaction.Commit();
    }
    catch (Exception ex)
    {
        // rollback cofa tranzakcję - cofa wszystkie zmiany w niej dokonane (od momentu rozpoczęcia)
        transaction.Rollback();
    }
}

```

## Zad10
w konsoli PackageManager (powershell z exekami od naszych bibliotek) piszemy `enable-migrations`, `add-migration InitialMigration`, (jest też `update-database` / `database update`)  

generują się pliki w folderze `Migrations` - klasa `InitialMigration` udostępniająca metody `Up` i `Down` (do aktualizacji i cofania zmian)