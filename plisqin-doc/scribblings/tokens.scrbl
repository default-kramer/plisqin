#lang scribble/manual

@(require (for-label plisqin)
          "tokens-skeleton.rkt"
          (except-in "tokens-custom-unsafe.rkt" doc)
          (except-in "tokens-custom-loose.rkt" doc)
          (except-in "tokens-custom-strict.rkt" doc))

@title[#:style '(toc)]{Token Constructors}

TODO explain the 3 variants. Explain the "type signatures".

TODO come up with a prefix convention and explain it.
I think I like @(racket %%exists) for unsafe and @(racket %exists) for loose.
But wait, if we have an SQL modulo operator what would it be?

@(table-of-contents)

@section{Unsafe}
@(defmodule plisqin-lib/unsafe)
@(custom-unsafe-docs)
@(docgen/unsafe)

@section{Loose}
@(defmodule plisqin-lib/loose)
@(custom-loose-docs)
@(docgen/loose)

@section{Strict}
@(defmodule plisqin-lib/strict)
@(custom-strict-docs)
@(docgen/strict)
