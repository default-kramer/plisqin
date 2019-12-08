#lang scribble/manual

@(require (for-label (prefix-in unsafe: plisqin-lib/unsafe/main))
          (for-label (prefix-in loose: plisqin-lib/loose/main))
          (for-label (prefix-in strict: plisqin-lib/strict/main))
          (for-label (prefix-in %% plisqin-lib/unsafe/main))
          (for-label (prefix-in % plisqin-lib/loose/main))
          "tokens-skeleton.rkt")

@title[#:style '(toc)]{Token Constructors}

TODO explain the 3 variants. Explain the "type signatures".

TODO come up with a prefix convention and explain it.
I think I like @(racket %%exists) for unsafe and @(racket %exists) for loose.
But wait, if we have an SQL modulo operator what would it be?

TODO testing the hyperlinks... remove this eventually
@(racketblock
  unsafe:exists
  loose:exists
  strict:exists)

@(table-of-contents)

@section{Unsafe}
@(docgen/unsafe)

@section{Loose}
@(docgen/loose)

@section{Strict}
@(docgen/strict)
