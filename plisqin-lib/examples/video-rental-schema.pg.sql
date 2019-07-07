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
	ItemTypeId serial primary key,
	ItemTypeName varchar(99) not null
);

create table Item (
	ItemId serial primary key,
	ItemTypeId integer not null references ItemType(ItemTypeId),
	ItemName varchar(99) not null,
	ReleaseDate timestamp
);

create table Copy (
	CopyId serial primary key,
	ItemId integer not null references Item(ItemId),
	Barcode varchar(80) not null unique
);

create table Genre (
	GenreId serial primary key,
	GenreName varchar(99) unique
);

create table ItemGenre (
	ItemId integer not null references Item(ItemId),
	GenreId integer not null references Genre(GenreId),
	primary key (ItemId, GenreId)
);

create table Account (
	AccountId serial primary key,
	PhoneNumber varchar(20) unique,
	Address varchar(99)
);

create table Customer (
	CustomerId serial primary key,
	AccountId integer not null references Account(AccountId),
	CustomerName varchar(99),
	CustomerBirthDate timestamp
);

create table District (
	DistrictId serial primary key,
	DistrictName varchar(99)
);

create table Store (
	StoreId serial primary key,
	DistrictId integer not null references District(DistrictId),
	Address varchar(99)
);

create table Employee (
	EmployeeId serial primary key,
	PrimaryStoreId integer not null references Store(StoreId),
	EmployeeName varchar(99)
);

create table Checkout (
	CheckoutId serial primary key,
	EmployeeId integer not null references Employee(EmployeeId),
	StoreId integer not null references Store(StoreId),
	CustomerId integer not null references Customer(CustomerId),
	CheckoutTime timestamp not null
);

create table Rental (
	RentalId serial primary key,
	CheckoutId integer not null references Checkout(CheckoutId),
	CopyId integer not null references Copy(CopyId),
	PricePaid decimal(10,2) not null
);