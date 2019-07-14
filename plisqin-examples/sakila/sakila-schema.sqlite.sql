-- Ported from MySQL to SQLite by Ryan Kramer.
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
  actor_id integer not null primary key autoincrement
  , first_name text not null
  , last_name text not null
  , last_update datetime_as_text not null
);

create table address (
  address_id integer not null primary key autoincrement
  , address1 text not null
  , address2 text null
  , district text not null
  , city_id integer not null
  , postal_code text null
  , phone text not null
  , last_update datetime_as_text not null
  , foreign key (city_id) references city (city_id)
);

create table category (
  category_id integer not null primary key autoincrement
  , category_name text not null
  , last_update datetime_as_text not null
);

create table city (
  city_id integer not null primary key autoincrement
  , city_name text not null
  , country_id integer not null
  , last_update datetime_as_text not null
  , foreign key (country_id) references country (country_id)
);

create table country (
  country_id integer not null primary key autoincrement
  , country_name text not null
  , last_update datetime_as_text not null
);

create table customer (
  customer_id integer not null primary key autoincrement
  , store_id integer not null
  , first_name text not null
  , last_name text not null
  , email text null
  , address_id integer not null
  , active boolean_as_integer null
  , create_date datetime_as_text not null
  , last_update datetime_as_text not null
  , foreign key (store_id) references store (store_id)
  , foreign key (address_id) references address (address_id)
);

create table film (
  film_id integer not null primary key autoincrement
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
  , last_update datetime_as_text not null
  , foreign key (language_id) references language (language_id)
  , foreign key (original_language_id) references language (language_id)
);

create table film_actor (
  actor_id integer not null
  , film_id integer not null
  , last_update datetime_as_text not null
  , primary key (actor_id, film_id)
  , foreign key (actor_id) references actor (actor_id)
  , foreign key (film_id) references film (film_id)
);

create table film_category (
  film_id integer not null
  , category_id integer not null
  , last_update datetime_as_text not null
  , primary key (film_id, category_id)
  , foreign key (film_id) references film (film_id)
  , foreign key (category_id) references category (category_id)
);

create table inventory (
  inventory_id integer not null primary key autoincrement
  , film_id integer not null
  , store_id integer not null
  , last_update datetime_as_text not null
  , foreign key (film_id) references film (film_id)
  , foreign key (store_id) references store (store_id)
);

create table language (
  language_id integer not null primary key autoincrement
  , language_name text not null
  , last_update datetime_as_text not null
);

create table payment (
  payment_id integer not null primary key autoincrement
  , customer_id integer not null
  , staff_id integer not null
  , rental_id integer null
  , amount real not null
  , payment_date datetime_as_text null
  , last_update datetime_as_text not null
  , foreign key (customer_id) references customer (customer_id)
  , foreign key (staff_id) references staff (staff_id)
  , foreign key (rental_id) references rental (rental_id)
);

create table rental (
  rental_id integer not null primary key autoincrement
  , rental_date datetime_as_text not null
  , inventory_id integer not null
  , customer_id integer not null
  , return_date datetime_as_text null
  , staff_id integer not null
  , last_update datetime_as_text not null
  , foreign key (inventory_id) references inventory (inventory_id)
  , foreign key (customer_id) references customer (customer_id)
  , foreign key (staff_id) references staff (staff_id)
);

create table staff (
  staff_id integer not null primary key autoincrement
  , first_name text not null
  , last_name text not null
  , address_id integer not null
  , picture blob null
  , email text null
  , store_id integer not null
  , active boolean_as_integer not null
  , username text not null
  , password text null
  , last_update datetime_as_text not null
  , foreign key (address_id) references address (address_id)
  , foreign key (store_id) references store (store_id)
);

create table store (
  store_id integer not null primary key autoincrement
  , manager_staff_id integer not null
  , address_id integer not null
  , last_update datetime_as_text not null
  , foreign key (manager_staff_id) references staff (staff_id)
  , foreign key (address_id) references address (address_id)
);