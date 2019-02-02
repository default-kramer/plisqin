#lang scribble/manual
@(require scribble/core
          scribble/html-properties
          "TEMP-fork-scribble-code-examples.rkt"
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
     (get-evaluation-results #:lang "plisqin" #:eval ex-eval
                             (format "(to-sql ~a)" code-query)))
   (match result
     [(list a) a]
     [else #f]))

@(define-syntax-rule (show-query str)
   ; 1) Show the code example.
   ; 2) Execute query against SQLite.
   ; 3) Show table of query result.
   (begin
     (codex str)
     (let ([sql (get-sql str)])
       (if (not (string? sql))
           (begin
             (displayln "WARNING to-sql failed")
             (displayln str)
             (void))
           (let* ([conn (connect-cities)]
                  [result (query conn sql)]
                  [_ (disconnect conn)])
             (to-table result))))))

First define the schema:

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

@(show-query #<<CODE
{from ct City
      {where ct.CityPopulation" > " 20 * 1000 * 1000}}
CODE
             )

Testing:

@(codex #<<CODE
(+ 1 2)
{if 41.add1 = 42
    "it sure does"
    'nope}
"another line of code"
CODE
        )
@(codex "{identity 41.add1}")
@(codex "{41.add1}")
@(codex "{3 + 4}")