#lang scribble/manual
@(require (for-label plisqin-lib
                     "racket.rkt"
                     plisqin-lib/examples/video-rental-schema))
@(require scribble/eval
          plisqin-lib
          rackunit
          "helpers.rkt"
          "racket.rkt")

@title[#:style '(toc)]{Walkthrough}
@(local-table-of-contents)

@(define-syntax-rule (interact forms ...)
   (interaction
    #:eval my-eval
    forms ...))

@(include-section "walkthrough-core.scrbl")

@(include-section "schema.scrbl")

@section{Layer 2 - SQL as Procedures}
Work in Progress!

The ideal version of Plisqin encodes most of SQL into procedures or macros.
For example, @(racket count) and @(racket exists) are completed, but @(racket like) is not.
Follow those links to see what else is available.

The biggest hindrance here is the differences between databases.
For example, it would be nice to provide a unified interface for datetime arithmetic,
but this would require careful consideration of how to design the API and what
should happen if an operation is not supported by the target database.