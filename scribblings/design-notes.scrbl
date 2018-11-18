#lang scribble/manual
@(require (for-label plisqin
                     "racket.rkt"
                     plisqin/examples/video-rental-schema))
@(require scribble/eval
          plisqin
          rackunit
          "helpers.rkt"
          "racket.rkt")

@title{Design Notes}

@section[#:tag "sources-need-uids"]{Why do sources need UIDs?}
For some reason I keep thinking "can I just have all UIDs be 0 by default and use something
like object equality?" And the answer is still NO. Consider this:

@(racketblock+eval
  #:eval my-eval
  (define (duplicates-of x)
    (from t "Title"
          (where t".PrimaryTitle = "x".PrimaryTitle")
          (where t".TitleID < "x".TitleID")))
  (from t "Title"
        (where (exists (duplicates-of t))))
  )

If all the sources are @(racket '(Source "Title" "t" 0)) then @(racket t) and @(racket x) will
refer to the same thing. This is clearly wrong.

@section{Dotted Identifiers}
Maybe the existing tooling around @(racket read-cdot) would be useful?

It would be nice to support @(racket rental:ItemName) and @(racket rental.ItemName) where
@(racket a:x) means @(racket (x a)) and @(racket a.x) means @(racket (verify-no-joins (x a))).
Here's a quick-and-dirty proof of concept that uses #%top to try to split unbound identifiers
into potentially bound ones:

@(interaction
  #:eval (make-eval)
  (require racket (for-syntax racket))
  (define-syntax (applicate stx)
    (syntax-case stx ()
      [(_ x) #'x]
      [(_ x f rest ...)
       #'(applicate (f x) rest ...)]))

  (applicate 0 sub1 sub1)

  (module help racket
    (provide str->stx split-id)
    (define (str->stx str ctx srcloc)
      (if (string->number str)
          (datum->syntax ctx
                         (string->number str)
                         srcloc)
          (datum->syntax ctx
                         (string->symbol str)
                         srcloc)))
    (define/contract (split-id x)
      (-> syntax? (or/c #f (listof syntax?)))
      (if (and (identifier? x)
               (not (identifier-binding x)))
          (code:comment "unbound id:")
          (let* ([str (~a (syntax->datum x))]
                 [parts (string-split str ".")])
            (match parts
              [(list) #f]
              [(list a) #f]
              [(list a ...)
               (map (λ(s) (str->stx s x x)) a)]))
          (code:comment "else:")
          #f)))
  (require 'help (for-syntax 'help))

  (define-syntax (#%top stx)
    (syntax-case stx ()
      [(_ . x)
       (if (and (identifier? #'x)
                (not (identifier-binding #'x)))
           (code:comment "unbound identifier, try to split:")
           (let ([parts (split-id #'x)])
             (if parts
                 #`(applicate #,@parts)
                 (code:comment "TODO it would be nice to hook into the existing #%top here...")
                 (raise-syntax-error #f "unbound identifier" #'x)))
           (code:comment "else, not an unbound identifier:")
           #'x)]))

  (code:comment "Existing bindings should be left alone")
  (define x 3)
  (define x.j 99)
  (code:comment "no binding for x.add1.add1 so it converts to (applicate x add1 add1)")
  x.add1.add1
  (code:comment "there is a binding for x.j so it is immediately returned)")
  x.j

  (* 3 10.add1.add1)
  (if 10.add1 'yes 'no)
  (if x.j 'yes 'no)
  (code:comment "syntax error:")
  (if x.k 'yes 'no))

@section{Name Clashes and Infix Notation}
This is an idea that could solve two problems.
@itemlist[
 @item{Most operators (like @(racket +) and @(racket =)) already have meaning from Racket.}
 @item{Most users of SQL prefer infix notation}]

We could make a special macro that rewrites expressions according to certain rules.
@(racketblock
  (magic
   (code:comment "this seems like it would be very palatable even if you don't like Lisp:")
   (from x "X"
         (where x.ID + 3 = 9)))
  (code:comment "the magic macro would rewrite this to:")
  (from x "X"
        (where (my= (my+ x.ID 3) 9))))

There must be prior work here, but here's a simple proof of concept:
@(interaction
  #:eval my-eval
  (require racket)
  (code:comment "rules is a list of procedures of type (-> syntax? (or/c syntax? #f))")
  (define rules '())
  (define-syntax-rule (defrules literals cases ...)
    (begin
      (define (f x)
        (syntax-case x literals
          cases
          ...
          [else #f]))
      (set! rules (cons f rules))))
  (define/contract (apply-rules x)
    (-> syntax? syntax?)
    (let* ([x (or (ormap (λ(rule) (rule x)) rules)
                  x)]
           [children (syntax->list x)])
      (if children
          (datum->syntax x
                         (map apply-rules children)
                         x x)
          x)))
  (code:comment "this would be built into Plisqin somewhere:")
  (define/contract (my= a b)
    (-> sql-token? sql-token? sql-token?)
    (bool a" = "b))
  (code:comment "ok, now let's define the rule:")
  (defrules (=)
    [(a = b) #'(my= a b)])
  (apply-rules #'(3 = "hi")))

Note that you can escape the rules simply by using a prefix or different name.
For example, if you really want @(racket (3 = "hi")) to refer to racket's @(racket =),
then you could use a prefix like @(racket (3 racket:= "hi"))

The other option for escaping would be use use unquote.
(Then you would have to double-unquote to get out of a quasiquote, but this seems fine.)

@(interaction
  #:eval my-eval
  (defrules ()
    [,x #'x])
  (apply-rules #'(3 = "hi"))
  (apply-rules #'(3 ,= "hi"))
  (apply-rules #'`(a ,b ,,c)))