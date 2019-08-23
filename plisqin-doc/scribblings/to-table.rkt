#lang racket

(provide to-table)

(require db
         scribble/manual
         scribble/core
         scribble/html-properties)

(define (path-from-here str)
  (simplify-path (build-path (syntax-source #'here)
                             ".."
                             str)))
(define P-props
  (list (make-css-addition (path-from-here "PStyles.css"))
        (make-js-addition (path-from-here "PScripts.js"))))
(define PQueryResults
  (make-style "PQueryResults" P-props))
(define PTableWrapper
  (make-style "PTableWrapper show-results" P-props))
(define PSql
  (make-style "PSql" P-props))
(define PShowSql
  (make-style "PShowSql" P-props))
(define PShowTable
  (make-style "PShowTable" P-props))

(define/contract (to-table result sql)
  (-> rows-result? string? table?) ; returns a scribble table

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
