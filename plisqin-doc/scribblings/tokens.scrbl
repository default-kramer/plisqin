#lang scribble/manual

@(require (for-label plisqin)
          "tokens-skeleton.rkt"
          (except-in "tokens-custom-unsafe.rkt" doc)
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
This variant is on hold.
I think it should use the same signatures as the strict variant, except that
instead of requiring the programmer to prove the absence of errors, the loose
variant requires nothing and only gets in the way when it can prove the
presence of an error.

Specifically, that means that supertypes should pass for subtypes.
A proc that wants a Number should accept a Scalar, because a Scalar
might actually be a Number that just lost type information somwhere.
However, a String is definitely not a Number and the loose variant would
raise an error in that case.

For nullability the strict variant typically treats "maybe" as "yes".
The loose variant would treat "maybe" as "no".

@section{Strict}
@(defmodule plisqin-lib/strict)
@(custom-strict-docs)
@(docgen/strict)
