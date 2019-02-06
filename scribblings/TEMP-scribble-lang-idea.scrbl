#lang scribble/manual
@(require (for-label plisqin)
          scribble/core
          scribble/html-properties
          racket/sandbox
          "scribble-code-examples-lib/main.rkt"
          "racket.rkt"
          "../examples/cities/city-connection.rkt"
          (except-in db statement?))

@(define PQueryResults
   (make-style "PQueryResults SCodeFlow" ; SCodeFlow for the left border
               (list (make-css-addition "PStyles.css"))))

@(define/contract (to-table result)
   (-> rows-result? table?) ; db result -> scribble table

   ; First element is column name, second is scribble properties
   (define col-info? (list/c string? any/c))

   (define/contract (get-column-info result)
     (-> rows-result? (listof col-info?))
     (map (λ(props)
            (let ([col-name (assq 'name props)]
                  [decltype (assq 'decltype props)])
              (list
               ; first - column name:
               (if col-name
                   (cdr col-name)
                   "[no column name]")
               ; second - scribble props:
               (if (and decltype
                        (string? (cdr decltype))
                        (member (string-downcase (cdr decltype)) '("integer" "real")))
                   ; numeric types align right:
                   'right
                   ; otherwise no style:
                   '()))))
          (rows-result-headers result)))

   (define/contract (get-rows result)
     (-> rows-result? (listof (listof content?)))
     (define rows (map vector->list (rows-result-rows result)))
     (map (λ(row) (map ~a row)) rows))

   (let ([col-info (get-column-info result)])
     (tabular
      #:style PQueryResults
      #:column-properties (map cdr col-info)
      (list* (map car col-info)
             (get-rows result)))))

@(define (make-eval)
   (define eval (make-code-eval #:lang "plisqin"))
   (eval '(require (for-label plisqin "racket.rkt")))
   ; TODO override the real show-table which doesn't exist yet
   (eval '(define show-table void))
   eval)
@(define ex-eval (make-eval))
@(define ex-here (ex-eval "#'here"))

@(define (codex str)
   ; Use define to force immediate evaluation!
   (define result
     (code-examples str #:lang "plisqin" #:context ex-here #:eval ex-eval))
   result)

@(define (get-sql code-query)
   ; Use define to force immediate evaluation!
   (define result
     (eval-only ex-eval
                (format "#lang plisqin \n (to-sql ~a)" code-query)))
   (match result
     [(list a)
      #:when (string? a)
      a]
     [else
      (println "WARNING to-sql failed")
      (println (get-error-output ex-eval))
      #f]))

@(define-syntax-rule (show-table str)
   ; NOPE) Show the code example.
   ; 2) Execute query against SQLite.
   ; 3) Show table of query result.
   (begin
     ;(codex str)
     (let ([sql (get-sql str)])
       (when (string? sql)
         (let* ([conn (connect-cities)]
                [result (query conn sql)]
                [_ (disconnect conn)])
           (to-table result))))))

@section{Query Fundamentals}

First define the schema:

@margin-note{
 Plisqin doesn't require you to define your schema up front.
 This is just the fastest way to demonstrate the coolest features.
}
@(codex #<<CODE
(def-table Country)
(def-table City)
(def-fields-of Country
  CountryId
  CountryName
  CountryPopulation)
(def-fields-of City
  CityId
  CountryId
  CityName
  CityPopulation)
CODE
        )

Now write a query:

@margin-note{
 If you prefer, you can write @(racket (CityPopulation ct))
 instead of @(racket ct.CityPopulation).
}
@(codex #<<CODE
(define (big-cities)
  {from ct City
        {order-by ct.CityPopulation" desc"}
        {limit 10}
        {select ct.CityName}
        {select ct.CityPopulation}})
(show-table (big-cities))
CODE
        )
@(show-table "(big-cities)")

Add a join:

@(codex #<<CODE
(define (TEMP-typeset-only)
  {from ct (big-cities)
        {join co Country
              {join-on co.CountryId = ct.CountryId}}
        {select co.CountryName}})
CODE
        )

Let's make the join reusable:

@(codex #<<CODE
(def/append! (Country x)
  [(City? x)
   {join co Country
         {join-on co.CountryId = x.CountryId}}])
CODE
        )

Now use it
@(codex #<<CODE
(define (big-cities-with-country)
  {from ct (big-cities)
        {select ct.Country.CountryName}})
(show-table (big-cities-with-country))
CODE
        )
@(show-table "(big-cities-with-country)")

Make CountryName available to Cities.
@(codex #<<CODE
(def/append! (CountryName x)
  [(City? x)
   {x.Country.CountryName}])
CODE
        )

Filter using where
@(codex #<<CODE
(define (big-cities-outside-china)
  {from ct (big-cities-with-country)
        {where ct.CountryName not-like "%china"}})
(show-table (big-cities-outside-china))
CODE
        )
@(show-table "(big-cities-outside-china)")

@section{Plural Joins}
@(codex #<<CODE
(def-table Organization)
(def-fields-of Organization
  OrgId
  OrgShortName)

(def-table CountryOrganization)
(def-fields-of CountryOrganization
  CountryId
  OrgId)

(def/append! (Countries x)
  [(Organization? x)
(begin ; TODO fix!
   (define mappers
     {join map CountryOrganization
           {join-on map.OrgId = x.OrgId}})
   {join co Country
         {join-on co.CountryId = mappers.CountryId}}
)])

(def/append! (Organizations x)
  [(Country? x)
(begin ; TODO fix!
   (define mappers
     {join map CountryOrganization
           {join-on map.CountryId = x.CountryId}})
   {join org Organization
         {join-on org.OrgId = mappers.OrgId}}
)])
CODE
        )

@(codex #<<CODE
(define (countries-and-orgs)
  {from co Country
        {join orgs co.Organizations}
        {select co.CountryName}
        {select orgs.OrgShortName}
        {order-by co.CountryName}
        {order-by orgs.OrgShortName}
        {limit 10}})
(show-table (countries-and-orgs))
CODE
        )
@(show-table "(countries-and-orgs)")

@section{Grouped Joins and Aggregates}

Define a grouped join:

@margin-note{
 @(racket CitiesG) means "a Group of Cities."
 The general naming convention is @italic{PluralNounsG} meaning "a Group of Plural Nouns."
}
@(codex #<<CODE
(def/append! (CitiesG x)
  [(Country? x)
   {join ct City
         {group-by ct.CountryId}
         {join-on ct.CountryId = x.CountryId}}])
CODE
        )

Now use it:

@margin-note{
 This query illustrates the reason for the @italic{PluralNounsG} naming convention.
 When you see a Group, it should be enclosed in an aggregate such as @(racket count) or @(racket sum).
}
@(codex #<<CODE
(define (city-stats-by-country)
  {from co Country
        {select co.CountryName}
        {select {count co.CitiesG}" as CountCitiesG"}
        {select {sum co.CitiesG.CityPopulation}" as SumCityPopulation"}
        {order-by {count co.CitiesG}" desc"}
        {limit 10}})
(show-table (city-stats-by-country))
CODE
        )
@(show-table "(city-stats-by-country)")

An Organization can also have a group of Cities.
@(codex #<<CODE
(def/append! (CitiesG x)
  [(Organization? x)
   {join ct City
         {group-by ct.Country.Organizations.OrgId}
         {join-on ct.Country.Organizations.OrgId = x.OrgId}}])
CODE
        )

Now we can show similar state
@(codex #<<CODE
(define (city-stats-by-org)
  {from org Organization
        {select org.OrgShortName}
        {select {count org.CitiesG}" as CountCitiesG"}
        {select {sum org.CitiesG.CityPopulation}" as SumCityPopulation"}
        {order-by {count org.CitiesG}" desc"}
        {limit 10}})
(show-table (city-stats-by-org))
CODE
        )
@(show-table "(city-stats-by-org)")

@section{A Taste of Power}
Point out the similarities between both versions.
Combine them into this one:

@(codex #<<CODE
(define (city-stats table)
  {from x table
        {select {count x.CitiesG}" as CountCitiesG"}
        {select {sum x.CitiesG.CityPopulation}" as SumCityPopulation"}
        {order-by {count x.CitiesG}" desc"}
        {limit 10}})
CODE
        )

@(codex #<<CODE
(define (city-stats-by-country)
  {from co (city-stats Country)
        {select co.CountryName}})
(show-table (city-stats-by-country))
CODE
        )

@(show-table "(city-stats-by-country)")
@(codex #<<CODE
(define (city-stats-by-org)
  {from org (city-stats Organization)
        {select org.OrgShortName}})
(show-table (city-stats-by-org))
CODE
        )
@(show-table "(city-stats-by-org)")

TODO guide what to read next.