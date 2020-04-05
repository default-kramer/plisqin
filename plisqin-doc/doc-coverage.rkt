#lang racket

(module+ test
  (require doc-coverage plisqin)
  (check-all-documented 'plisqin))
