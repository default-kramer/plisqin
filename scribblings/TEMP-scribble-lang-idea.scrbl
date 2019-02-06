#lang scribble/manual
@(require scribble/core
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

@(codex #<<CODE
(define (big-cities)
  {from ct City
        {order-by ct.CityPopulation" desc"}
        {limit 10}})
(show-table (big-cities))
CODE
        )
@(show-table "(big-cities)")

Write a different query:
@(codex #<<CODE
(define (aa-cities)
  {from ct City
        {where ct.CityName like "aa%"}})
(show-table (aa-cities))
CODE
        )
@(show-table "(aa-cities)")

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