#lang racket

; What's the plan this time around? Maybe
#;(void
   (core "Most general data model"
         "Only layer that is allowed to touch Morsel?"
         "Also, utility functions as needed")
   (sql #:depends-on [core]
        "Implement SQL Expressions such as "(from join avg like = + * etc...)
        "Comes in 3 variations: unsafe, loose, and strict")
   (schema #:depends-on [core]
           "Implement def-table, def/append! and define-schema"))
