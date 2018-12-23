#lang racket

(provide postgres postgres? mssql mssql?
         dialect? current-dialect)

(define dialect?
  (or/c 'postgres 'mssql #f))

(define-syntax-rule (def-dialect name name?)
  (begin
    (define (name) 'name)
    (define (name? x)
      (equal? 'name x))))
(def-dialect postgres postgres?)
(def-dialect mssql mssql?)

(define current-dialect (make-parameter #f))