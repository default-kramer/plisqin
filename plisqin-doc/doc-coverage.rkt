#lang racket

(module+ test
  (require doc-coverage
           plisqin
           plisqin-examples/adventure-works)
  (check-all-documented 'plisqin)
  (check-all-documented 'plisqin-examples/adventure-works)
  )
