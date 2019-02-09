#lang scribble/manual
@(require (for-label plisqin)
          scribble/core
          scribble/html-properties
          racket/sandbox
          "scribble-code-examples-lib/main.rkt"
          "racket.rkt"
          "../examples/cities/city-connection.rkt"
          (except-in db statement?))

@(define P-props (list (make-css-addition "PStyles.css")
                       (make-js-addition "PScripts.js")))
@(define PQueryResults
   (make-style "PQueryResults" P-props))
@(define PTableWrapper
   (make-style "PTableWrapper show-results" P-props))
@(define PSql
   (make-style "PSql" P-props))
@(define PShowSql
   (make-style "PShowSql" P-props))
@(define PShowTable
   (make-style "PShowTable" P-props))

@(define/contract (to-table result sql)
   (-> rows-result? string? table?) ; db result -> scribble table

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
      #:style PTableWrapper
      (list
       (list (elem
              (elem #:style PShowTable "Show Table")
              (elem #:style PShowSql "Show SQL")))
       (list (tabular #:style PSql
                      (list (list (verbatim sql)))))
       (list
        (tabular
         #:style PQueryResults
         #:column-properties (map cdr col-info)
         (list* (map car col-info)
                (get-rows result))))))))

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
           (to-table result sql))))))

@section{Setup and Schema}

@margin-note{
 Plisqin doesn't require you to define your schema up front,
 but this is the fastest way to demonstrate the coolest features.
}
TODO explain.

@(codex #<<CODE
(def-table Country)
(def-table City)
(def-table Organization)
; a mapping table for the Many:Many relationship of Country:Organization
(def-table CountryOrganization)

(def-fields-of Country
  CountryId
  CountryName
  CountryPopulation)

(def-fields-of City
  CityId
  CountryId
  CityName
  CityPopulation)

(def-fields-of Organization
  OrgId
  OrgShortName)

(def-fields-of CountryOrganization
  CountryId
  OrgId)
CODE
        )

@section{Query Fundamentals}

The @(racket from) macro is used to create a query.
The following code defines a function called @(racket big-cities) which
takes no arguments and returns a query:
@margin-note{
 Love S-expressions? Don't worry, the syntactic sugar is optional.
 You can write @(racket (CityName city)) instead.
}
@(codex #<<CODE
(define (big-cities)
  {from city City
        {order-by 'desc city.CityPopulation}
        {limit 10}
        {select city.CityName}
        {select city.CityPopulation}})
CODE
        )

The above query has 4 clauses: an @(racket order-by) clause, a @(racket limit)
clause, and 2 @(racket select) clauses.

You can pass a query into @(racket show-table) and it will print the results:
@(codex "(show-table (big-cities))")
@(show-table "(big-cities)")

Queries are appendable. This means you can add more clauses to
a query you already defined elsewhere.
For example, let's start with @(racket big-cities) and append a
@(racket where) clause that excludes cities with a population over 20 million:

@(codex #<<CODE
(show-table
  {from city (big-cities)
        {where city.CityPopulation <= 20 * 1000 * 1000}})
CODE
        )
@(show-table "{from city (big-cities) {where city.CityPopulation < 20 * 1000 * 1000}}")

It's important to understand that the above query has 5 clauses in total:
the @(racket where) clause plus the 4 clauses from @(racket big-cities).
You might be able to foresee that appendable queries can greatly reduce code duplication.

@section{Joins}

Every City has exactly one Country.
Relationships between tables are represented using @(racket join).
The following query appends to @(racket big-cities), joins the Country table,
and adds a @(racket select) clause.

@(codex #<<CODE
(define (TEMP-typeset-only)
  {from city (big-cities)
        {join country Country
              {join-on country.CountryId = city.CountryId}}
        {select country.CountryName}})
CODE
        )

The above query would work just fine, but joining the Country
of a City is something we will be doing often.
So let's make it reusable:

@(codex #<<CODE
(def/append! (Country x)  ; the Country of x ...
  [(City? x)              ; if x is a City ...
   ; ... is this join:
   {join country Country
         {join-on country.CountryId = x.CountryId}}])
CODE
        )

The definition of @(racket Country) has been expanded.
In addition to being a table, @(racket Country) can now be used as
a function that takes a City and returns a join to the City's Country.
So now we can use @(racket {city.Country}) in our queries:
@(codex #<<CODE
(define (big-cities-with-country)
  {from city (big-cities)
        {select city.Country.CountryName}})
(show-table (big-cities-with-country))
CODE
        )
@(show-table "(big-cities-with-country)")

That's pretty nice, but we can do even better.
Do we really want to say @(racket {city.Country.CountryName}) every time?
Can we just say @(racket {city.CountryName}) instead?
Yes, by expanding the definition of @(racket CountryName):

@(codex #<<CODE
(def/append! (CountryName x)  ; the CountryName of x ...
  [(City? x)                  ; if x is a City ...
   ; ... is this expression:
   {x.Country.CountryName}])
CODE
        )

And that should do it. Let's try it out:
@(codex #<<CODE
(define (big-cities-outside-china)
  {from city (big-cities-with-country)
        {where city.CountryName not-like "%china"}})
(show-table (big-cities-outside-china))
CODE
        )
@(show-table "(big-cities-outside-china)")

@section{Timeout for more Schema}
Let's take a peek at the Organization table:
@(codex "(show-table {from org Organization})")
@(show-table "{from org Organization}")

A Country can have multiple Organizations.
For example, Belgium is a member of both the EU and NATO.
Obviously, an Organization can have multiple Countries.
This many-to-many relationship requires a mapping table, CountryOrganization.

@(codex #<<CODE
(def/append! (CountryOrganizations x)
  [(Country? x)
   {join co CountryOrganization
         {join-on co.CountryId = x.CountryId}}]
  [(Organization? x)
   {join co CountryOrganization
         {join-on co.OrgId = x.OrgId}}])
(def/append! (Organizations x)
  [(Country? x)
   {join org Organization
         {join-on org.OrgId = x.CountryOrganizations.OrgId}}])
(def/append! (Countries x)
  [(Organization? x)
   {join country Country
         {join-on country.CountryId = x.CountryOrganizations.CountryId}}])
CODE
        )

Now we are able to use @(racket {country.Organizations}) and
@(racket {organization.Countries}) in our queries.
Let me just prove what I said about Belgium is true:
@(codex #<<CODE
(define (belgium-orgs)
  {from co Country
        {where co.CountryName = "Belgium"}
        {select co.CountryName}
        {select co.Organizations.OrgShortName}})
(show-table (belgium-orgs))
CODE
        )
@(show-table "(belgium-orgs)")

@section{Grouped Joins and Aggregates}

Consider this code:

@margin-note{
 @(racket CitiesG) means "a Group of Cities."
 The general naming convention is @italic{PluralNounsG} meaning "a Group of Plural Nouns."
}
@(codex #<<CODE
(def/append! (CitiesG x)
  [(Country? x)
   {join city City
         {group-by city.CountryId}
         {join-on city.CountryId = x.CountryId}}])
CODE
        )

I call @(racket CitiesG) a "grouped join."
An important quality of a grouped join is that it does not increase the cardinality
of the result set with respect to its argument(s).
In the above example, the argument @(racket x) is tested to be a Country.
The join is grouped by CountryId.
Therefore, there will be at most 1 group per Country.

Grouped joins are used with aggregate functions.
In the following example, @(racket count) and @(racket sum) are the aggregates.
Notice that @(racket CitiesG) always occurs inside an aggregate:

@margin-note{
 TODO it would probably be good to mention that grouping and aggregating
 is compatible with the SQL way of doing it.
}
@(codex #<<CODE
(define (city-stats-by-country)
  {from co Country
        {select co.CountryName}
        {select {count co.CitiesG}" as CountCitiesG"}
        {select {sum co.CitiesG.CityPopulation}" as SumCityPopulation"}
        {order-by 'desc {count co.CitiesG}}
        {limit 10}})
(show-table (city-stats-by-country))
CODE
        )
@(show-table "(city-stats-by-country)")

OK, so we are seeing aggregated City information by Country.
Could we not also see aggregated City information by Organization?
Yes, because just like a Country, an Organization also has a group of Cities.
So first, let's extend the definition of @(racket CitiesG):

@(codex #<<CODE
(def/append! (CitiesG x)
  [(Organization? x)
   {join ct City
         {group-by ct.Country.Organizations.OrgId}
         {join-on ct.Country.Organizations.OrgId = x.OrgId}}])
CODE
        )

The above join is a valid grouped join because it will return at most one
group of Cities per Organization. (Note that a single City might belong to
more than one Organization, but that is fine.)

Now if we review @(racket city-stats-by-country), we see that the only
Country-specific clause is @(racket {select co.CountryName}).
Let's remove that clause to make a reusable version:
@(codex #<<CODE
(define (city-stats X)
  {from x X
        {select {count x.CitiesG}" as CountCitiesG"}
        {select {sum x.CitiesG.CityPopulation}" as SumCityPopulation"}
        {order-by 'desc {count x.CitiesG}}
        {limit 10}})
CODE
        )

Now @(racket city-stats) should work on any X for which @(racket CitiesG) is defined!
Which makes it easy to implement the "by Country" and "by Organization" variants:

@(codex #<<CODE
(define (city-stats-by-country)
  {from co (city-stats Country)
        {select co.CountryName}
        {select co.CountryId}})
(define (city-stats-by-org)
  {from org (city-stats Organization)
        {select org.OrgShortName}})
CODE
        )

@(codex "(show-table (city-stats-by-country))")
@(show-table "(city-stats-by-country)")

@(codex "(show-table (city-stats-by-org))")
@(show-table "(city-stats-by-org)")

TODO guide what to read next.