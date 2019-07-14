-- Ported from MySQL to SQL Server by Ryan Kramer.
-- Original License and version information follows:
-- Sakila Sample Database Schema
-- Version 1.0

-- Copyright (c) 2006, 2015, Oracle and/or its affiliates. 
-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

--  * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
--  * Neither the name of Oracle nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

create table actor (
  actor_id integer not null primary key identity
  , first_name text not null
  , last_name text not null
  , last_update datetime not null
);

create table address (
  address_id integer not null primary key identity
  , address1 text not null
  , address2 text null
  , district text not null
  , city_id integer not null
  , postal_code text null
  , phone text not null
  , last_update datetime not null
);

create table category (
  category_id integer not null primary key identity
  , category_name text not null
  , last_update datetime not null
);

create table city (
  city_id integer not null primary key identity
  , city_name text not null
  , country_id integer not null
  , last_update datetime not null
);

create table country (
  country_id integer not null primary key identity
  , country_name text not null
  , last_update datetime not null
);

create table customer (
  customer_id integer not null primary key identity
  , store_id integer not null
  , first_name text not null
  , last_name text not null
  , email text null
  , address_id integer not null
  , active bit null
  , create_date datetime not null
  , last_update datetime not null
);

create table film (
  film_id integer not null primary key identity
  , title text not null
  , description text null
  , release_year integer null
  , language_id integer not null
  , original_language_id integer null
  , rental_duration integer not null
  , rental_rate real not null
  , length integer null
  , replacement_cost real not null
  , rating text not null
  , special_features text null
  , last_update datetime not null
);

create table film_actor (
  actor_id integer not null
  , film_id integer not null
  , last_update datetime not null
  , primary key (actor_id, film_id)
);

create table film_category (
  film_id integer not null
  , category_id integer not null
  , last_update datetime not null
  , primary key (film_id, category_id)
);

create table inventory (
  inventory_id integer not null primary key identity
  , film_id integer not null
  , store_id integer not null
  , last_update datetime not null
);

create table language (
  language_id integer not null primary key identity
  , language_name text not null
  , last_update datetime not null
);

create table payment (
  payment_id integer not null primary key identity
  , customer_id integer not null
  , staff_id integer not null
  , rental_id integer null
  , amount real not null
  , payment_date datetime null
  , last_update datetime not null
);

create table rental (
  rental_id integer not null primary key identity
  , rental_date datetime not null
  , inventory_id integer not null
  , customer_id integer not null
  , return_date datetime null
  , staff_id integer not null
  , last_update datetime not null
);

create table staff (
  staff_id integer not null primary key identity
  , first_name text not null
  , last_name text not null
  , address_id integer not null
  , picture varbinary(max) null
  , email text null
  , store_id integer not null
  , active bit not null
  , username text not null
  , password text null
  , last_update datetime not null
);

create table store (
  store_id integer not null primary key identity
  , manager_staff_id integer not null
  , address_id integer not null
  , last_update datetime not null
);

alter table address add foreign key (city_id) references city (city_id);

alter table city add foreign key (country_id) references country (country_id);

alter table customer add foreign key (store_id) references store (store_id);

alter table customer add foreign key (address_id) references address (address_id);

alter table film add foreign key (language_id) references language (language_id);

alter table film add foreign key (original_language_id) references language (language_id);

alter table film_actor add foreign key (actor_id) references actor (actor_id);

alter table film_actor add foreign key (film_id) references film (film_id);

alter table film_category add foreign key (film_id) references film (film_id);

alter table film_category add foreign key (category_id) references category (category_id);

alter table inventory add foreign key (film_id) references film (film_id);

alter table inventory add foreign key (store_id) references store (store_id);

alter table payment add foreign key (customer_id) references customer (customer_id);

alter table payment add foreign key (staff_id) references staff (staff_id);

alter table payment add foreign key (rental_id) references rental (rental_id);

alter table rental add foreign key (inventory_id) references inventory (inventory_id);

alter table rental add foreign key (customer_id) references customer (customer_id);

alter table rental add foreign key (staff_id) references staff (staff_id);

alter table staff add foreign key (address_id) references address (address_id);

alter table staff add foreign key (store_id) references store (store_id);

alter table store add foreign key (manager_staff_id) references staff (staff_id);

alter table store add foreign key (address_id) references address (address_id);
