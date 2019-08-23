#lang racket
(require "../private/util.rkt" plisqin-lib)
(provide (all-defined-out))

; This file contains the definitions for the Video Rental example schema
; that are used in most of the documentation. This also captures the syntax objects
; of the definitions so that we can place the definitions into the scribble docs.

(define-syntax-rule (define/capture ARGS ...)
  (doc/capture ARGS ...))

(define/capture (def-table) (def-table NAME _ ...) NAME
  (def-table Account "Account" "_account")
  (def-table Checkout "Checkout" "_checkout")
  (def-table Copy "Copy" "_copy")
  (def-table Customer "Customer" "_customer")
  (def-table District "District" "_district")
  (def-table Employee "Employee" "_employee")
  (def-table Genre "Genre" "_genre")
  (def-table Item "Item" "_item")
  (def-table ItemType "ItemType" "_itemType")
  (def-table ItemGenre "ItemGenre" "_itemGenre")
  (def-table Rental "Rental" "_rental")
  (def-table Store "Store" "_store"))

(define/capture (def/append!) (def/append! (NAME _) _ ...) NAME
  ; Singular relationships
  (def/append! (Account-of/s x)
    [(Customer? x)
     (RS join a (Account)
         (join-on a".AccountId = "x".AccountId"))]
    [(Checkout? x)
     (Account-of/s (Customer-of/s x))]
    [(Rental? x)
     (Account-of/s (Checkout-of/s x))]
    [(Account? x) x])
  (def/append! (Checkout-of/s x)
    [(Checkout? x) x]
    [(Rental? x)
     (RS join c (Checkout)
         (join-on c".CheckoutId = "x".CheckoutId"))])
  (def/append! (Copy-of/s x)
    [(Rental? x)
     (RS join c (Copy)
         (join-on c".CopyId = "x".CopyId"))]
    [(Copy? x) x])
  (def/append! (Customer-of/s x)
    [(Customer? x) x]
    [(Rental? x) (Customer-of/s (Checkout-of/s x))]
    [(Checkout? x)
     (RS join c (Customer)
         (join-on c".CustomerId = "x".CustomerId"))])
  (def/append! (Item-of/s x)
    [(Copy? x)
     (RS join i (Item)
         (join-on i".ItemId = "x".ItemId"))]
    [(Rental? x)
     (Item-of/s (Copy-of/s x))]
    [(Item? x) x])
  (def/append! (Store-of/s x)
    [(or (Checkout? x)
         (Employee? x))
     (RS join s (Store)
         (join-on s".StoreId = "x".StoreId"))]
    [(or (Rental? x)
         (Copy? x))
     (Store-of/s (Checkout-of/s x))]
    [(Store? x) x])

  ; Plural relationships
  (def/append! (Checkouts-of/p x)
    [(Employee? x)
     (RS join c (Checkout)
         (join-on c".EmployeeId = "x".EmployeeId"))]
    [(Store? x)
     (RS join c (Checkout)
         (join-on c".StoreId = "x".StoreId"))])
  (def/append! (Copies-of/p x)
    [(Item? x)
     (RS join c (Copy)
         (join-on c".ItemId = "x".ItemId"))])
  (def/append! (Genres-of/p x)
    [(Item? x)
     (RS join g (Genre)
         (join-on g".GenreId = "(ItemGenres-of/p x)".GenreId"))]
    [(or (Copy? x)
         (Rental? x))
     (Genres-of/p (Item-of/s x))])
  (def/append! (ItemGenres-of/p x)
    [(Item? x)
     (RS join ig (ItemGenre)
         (join-on ig".ItemId = "x".ItemId"))]
    [(Genre? x)
     (RS join ig (ItemGenre)
         (join-on ig".GenreId = "x".GenreId"))]
    [(or (Copy? x)
         (Rental? x))
     (ItemGenres-of/p (Item-of/s x))])

  ; Grouped joins
  (def/append! (Copies-of/g x)
    [(Item? x)
     (RS join c (Copy)
         (group-by (ItemId c))
         (join-on (ItemId c)" = "(ItemId x)))])
  (def/append! (Rentals-of/g x)
    [(Customer? x)
     (RS join r (Rental)
         (group-by (CustomerId r))
         (join-on (CustomerId r)" = "(CustomerId x)))]
    [(Copy? x)
     (RS join r (Rental)
         (group-by (CopyId r))
         (join-on (CopyId r)" = "(CopyId x)))]
    [(Item? x)
     (RS join r (Rental)
         (group-by (ItemId r))
         (join-on (ItemId r)" = "(ItemId x)))])

  ; Scalars
  (def/append! (Address x)
    [(Store? x)
     (RS scalar x".Address")])
  (def/append! (CheckoutTime x)
    [(Checkout? x)
     (RS scalar x".CheckoutTime")]
    [(Rental? x)
     (CheckoutTime (Checkout-of/s x))])
  (def/append! (CopyId x)
    [(or (Copy? x)
         (Rental? x))
     (RS scalar x".CopyId")]
    [(Item? x)
     (CopyId (Copy-of/s x))])
  (def/append! (CustomerBirthDate x)
    [(Customer? x)
     (RS scalar x".CustomerBirthDate")]
    [(or (Checkout? x)
         (Rental? x))
     (CustomerBirthDate (Customer-of/s x))])
  (def/append! (CustomerId x)
    [(or (Customer? x)
         (Checkout? x))
     (RS scalar x".CustomerId")]
    [(Rental? x)
     (CustomerId (Checkout-of/s x))])
  (def/append! (ItemId x)
    [(or (Item? x)
         (Copy? x))
     (RS scalar x".ItemId")]
    [(Rental? x)
     (ItemId (Copy-of/s x))])
  (def/append! (ItemTypeId x)
    [(or (ItemType? x)
         (Item? x))
     (RS scalar x".ItemTypeId")]
    [(or (Copy? x)
         (Rental? x))
     (ItemTypeId (Item-of/s x))])
  (def/append! (ReleaseDate x)
    [(Item? x)
     (RS scalar x".ReleaseDate")]
    [(or (Copy? x)
         (Rental? x))
     (ReleaseDate (Item-of/s x))])
  (def/append! (RentalId x)
    [(Rental? x)
     (RS scalar x".RentalId")])
  (def/append! (StoreId x)
    [(Store? x)
     (RS scalar x".StoreId")]
    [(or (Rental? x)
         (Checkout? x))
     (StoreId (Store-of/s x))]))

; Aliases, so I don't have to interrupt my sales pitch explaining of/s and friends.
(define Item-of Item-of/s)
(define grouped-Rentals-of Rentals-of/g)
(define Genres-of Genres-of/p)
