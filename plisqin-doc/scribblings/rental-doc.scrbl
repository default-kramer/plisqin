#lang scribble/manual
@(require racket)
@(require plisqin-lib/examples/video-rental-schema)
@(require (for-label plisqin-lib
                     plisqin-lib/examples/video-rental-schema
                     "racket.rkt"))
@(require "helpers.rkt")

@title{Video Rental Example Schema}
@(defmodule plisqin-lib/examples/video-rental-schema)

This section documents an example database schema that models a brick-and-mortar
video rental store.
Each table has an explanation of what it means.
Joins and fields typically just show the code.

@hyperlink["https://github.com/default-kramer/plisqin/raw/master/examples/video-rental-diagram.PNG"]{This diagram}
shows the database model.

@(define-syntax-rule (def-tableN TABLE TABLE? BODY ...)
   (deftogether(
                (defproc(TABLE [alias string?])
                  source?)
                (defproc(TABLE? [x any/c])
                  boolean?)
                ) (rb TABLE) BODY ...))

@(define-syntax (def-table stx)
   (syntax-case stx ()
     [(def-table TABLE BODY ...)
      (with-syntax ([TABLE? (datum->syntax stx (string->symbol (format "~a?" (syntax->datum #'TABLE))))])
        #'(def-tableN TABLE TABLE? BODY ...))]))

@(define-syntax-rule (def-singular NAME NAME? BODY ...)
   @(defproc
      (NAME [x any/c])
      NAME?
      (rb NAME)
      BODY ...))
@(define-syntax-rule (def-plural x ...)
   (def-singular x ...))
@(define-syntax-rule (def-group x ...)
   (def-singular x ...))

@section[#:tag "video-rental-tables"]{Tables}

@def-table[Account]{
 An account is a collection of 1 or more @(racket Customer)s.
 They probably share a household, but we don't care.
}
@def-table[Checkout]{
 A Checkout is a point-of-sale transaction.
}
@def-table[Copy]{
 A Copy is an instance of an @(racket Item).
 Each Copy has a barcode that is unique throughout the whole company.
 A @(racket Rental) points to a specific @(racket Copy).
 Obviously, a Copy can be rented multiple times.
}
@def-table[Customer]{
 A Customer is a person who has an @(racket Account).
}
@def-table[District]{
 A District is a collection of @(racket Store)s.
}
@def-table[Employee]{
 An Employee has 1 primary @(racket Store), even though they may work at other Stores if needed.
}
@def-table[Genre]{
 For example, "Action" and "Comedy" are genres.
 Every @(racket Item) can have multiple genres.
}
@def-table[Item]{
 An Item is @italic{the definition} something that can be rented or sold, typically a movie.
 A @(racket Copy) is @italic{an instance} of an Item.
 For example, "Role Models / DVD / 2009" would be an Item, and there would
 be multiple copies available to rent, each with their own barcode.
}
@def-table[ItemType]{
 An ItemType is a property of an @(racket Item).
 Examples might be "DVD", "BluRay", and "PS2 game".
}
@def-table[ItemGenre]{
 This is just the mapping table for the many:many relationship of @(racket Item):@(racket Genre).
}
@def-table[Rental]{
 A Rental is a line item of a @(racket Checkout).
 It has exactly 1 @(racket Copy) that is being rented.
 It also has exactly 1 @(racket Checkout).
}
@def-table[Store]{
 A Store is a physical location that rents stuff.
}

@(define-syntax-rule (doc-field NAME)
   @defproc[(NAME [x any/c])
            fragment?]{
 @(rb NAME)
 })

@section[#:tag "video-rental-singulars"]{Singular Relationships}
@def-singular[Account-of/s Account?]
@def-singular[Checkout-of/s Checkout?]
@def-singular[Copy-of/s Copy?]
@def-singular[Customer-of/s Customer?]
@def-singular[Item-of/s Item?]
@defproc[(Item-of [x any/c]) Item?]{
 An alias of @(racket Item-of/s) so I don't have to interrupt my sales pitch explaining @tech{of/s}.}

@section[#:tag "video-rental-plurals"]{Plural Relationships}
@def-plural[Checkouts-of/p Checkout?]
@def-plural[Copies-of/p Copy?]
@def-plural[Genres-of/p Genre?]
@defproc[(Genres-of [x any/c]) Genre?]{
 An alias of @(racket Genres-of/p) so I don't have to interrupt my sales pitch explaining @tech{of/p}.}
@def-plural[ItemGenres-of/p ItemGenre?]
@def-plural[Store-of/s Store?]

@section[#:tag "video-rental-groups"]{Grouped Relationships}
@def-group[Copies-of/g Copy?]
@def-group[Rentals-of/g Rental?]
@defproc[(grouped-Rentals-of [x any/c]) Rental?]{
 An alias of @(racket Rentals-of/g) so I don't have to interrupt my sales pitch explaining @tech{of/g}.}

@section[#:tag "video-rental-fields"]{Fields}
@doc-field[Address]
@doc-field[CheckoutTime]
@doc-field[CopyId]
@doc-field[CustomerBirthDate]
@doc-field[CustomerId]
@doc-field[ItemId]
@doc-field[ItemTypeId]
@doc-field[ReleaseDate]
@doc-field[RentalId]
@doc-field[StoreId]