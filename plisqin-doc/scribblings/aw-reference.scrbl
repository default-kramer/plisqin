#lang scribble/manual

@(begin
   (require (for-label plisqin-examples/adventure-works
                       "standard-label.rkt"))
   (require "helpers.rkt")
   )

@title{plisqin-examples/adventure-works}
@(defmodule plisqin-examples/adventure-works)

@defproc[(show-table [x (or/c query? string?)]) any/c]{
 Executes a query (or raw SQL) against the Adventure Works sample database.
 @(repl-query
   (aw:show-table "select ProductCategoryID, Name from ProductCategory"))
}

@defproc[(connect-adventure-works) db:connection?]{
 Creates a connection to the Adventure Works sample database.
 @(repl
   (let* ([conn (aw:connect-adventure-works)]
          [stmt "select ProductCategoryID, Name from ProductCategory"]
          [rows (db:query-rows conn stmt)])
     (db:disconnect conn)
     rows))
}
