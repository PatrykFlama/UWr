


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
