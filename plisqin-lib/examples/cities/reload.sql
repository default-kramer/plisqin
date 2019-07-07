-- To rebuild the DB, delete cities.db then run
--   sqlite3.exe cities.db < reload.sql

create table ImportCountries (
  CountryName text,
  CountryPopulation integer,
  NominalGDP real,
  LargestCityName text,
  LargestCityPop real
);

create table ImportCities (
  CityName text,
  CountryName text,
  CityPopulation integer
);

create table ImportOrgs (
  OrgShortName text,
  CountryName text
);

.separator ,
.import ./wikidata/countries.csv ImportCountries
.import ./wikidata/top-10000-cities.csv ImportCities
.import ./wikidata/orgs.csv ImportOrgs

create table Country (
  CountryId integer primary key not null,
  CountryName text not null,
  CountryPopulation real,
  NominalGDP real
);

create table City (
  CityId integer primary key not null,
  CountryId integer not null,
  CityName text not null,
  CityPopulation integer,
  foreign key (CountryId) references Country(CountryId)
);

create table Organization (
  OrgId integer primary key not null,
  OrgShortName text not null
);

create table CountryOrganization (
  CountryId integer not null,
  OrgId integer not null,
  primary key (CountryId, OrgId),
  foreign key (CountryId) references Country(CountryId),
  foreign key (OrgId) references Organization(OrgId)
);


-- Load countries
insert into Country(CountryName, CountryPopulation, NominalGDP)
select CountryName, CountryPopulation, NominalGDP from ImportCountries;


-- Load cities
insert into City(CountryId, CityName, CityPopulation)
select co.CountryId, ci.CityName, ci.CityPopulation
from ImportCities ci
inner join Country co on co.CountryName = ci.CountryName;

-- We could load the largest city for each country that doesn't have any cities
-- at this point. But having an empty set may be useful, so leave it out.


-- Load orgs
insert into Organization(OrgShortName)
select distinct OrgShortName from ImportOrgs;


-- Load Org <-> Country mappings
insert into CountryOrganization(CountryId, OrgId)
select co.CountryId, org.OrgId
from ImportOrgs io
inner join Country co on co.CountryName = io.CountryName
inner join Organization org on org.OrgShortName = io.OrgShortName;


drop table ImportCities;
drop table ImportCountries;
drop table ImportOrgs;