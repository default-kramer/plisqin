/*
drop table Rental;
drop table Checkout;
drop table Employee;
drop table Store;
drop table District;
drop table Customer;
drop table Account;
drop table ItemGenre;
drop table Genre;
drop table Copy;
drop table Item;
drop table ItemType;
*/

create table ItemType (
	ItemTypeId integer identity primary key,
	ItemTypeName varchar(99) not null
);

create table Item (
	ItemId integer identity primary key,
	ItemTypeId integer not null foreign key references ItemType(ItemTypeId),
	ItemName varchar(99) not null,
	ReleaseDate datetime
);

create table Copy (
	CopyId integer identity primary key,
	ItemId integer not null foreign key references Item(ItemId),
	Barcode varchar(80) not null unique
);

create table Genre (
	GenreId integer identity primary key,
	GenreName varchar(99) unique
);

create table ItemGenre (
	ItemId integer not null foreign key references Item(ItemId),
	GenreId integer not null foreign key references Genre(GenreId),
	primary key (ItemId, GenreId)
);

create table Account (
	AccountId integer identity primary key,
	PhoneNumber varchar(20) unique,
	Address varchar(99)
);

create table Customer (
	CustomerId integer identity primary key,
	AccountId integer not null foreign key references Account(AccountId),
	CustomerName varchar(99),
	CustomerBirthDate datetime
);

create table District (
	DistrictId integer identity primary key,
	DistrictName varchar(99)
);

create table Store (
	StoreId integer identity primary key,
	DistrictId integer not null foreign key references District(DistrictId),
	Address varchar(99)
);

create table Employee (
	EmployeeId integer identity primary key,
	PrimaryStoreId integer not null foreign key references Store(StoreId),
	EmployeeName varchar(99)
);

create table Checkout (
	CheckoutId integer identity primary key,
	EmployeeId integer not null foreign key references Employee(EmployeeId),
	StoreId integer not null foreign key references Store(StoreId),
	CustomerId integer not null foreign key references Customer(CustomerId),
	CheckoutTime datetime not null
);

create table Rental (
	RentalId integer identity primary key,
	CheckoutId integer not null foreign key references Checkout(CheckoutId),
	CopyId integer not null foreign key references Copy(CopyId),
	PricePaid decimal(10,2) not null
);