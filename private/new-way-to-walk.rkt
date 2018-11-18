#lang racket
(module+ test
  (require rackunit))

(provide walk return replace-multi delete-node no-recurse
         (rename-out [result? return?]))

(struct nothing ())

(define node? any/c)
(define accum? any/c)

(define (callback? x)
  (or (not x)
      (and (procedure? x)
           (member (procedure-arity x) '(1 2 3)))))

(define (ensure contract)
  (λ(x) (if (not (contract x))
            (error "TODO")
            x)))

(define enter-callback (make-parameter #f (ensure callback?)))
(define exit-callback (make-parameter #f (ensure callback?)))
(define accumulator (make-parameter (nothing)))

; List of parent nodes, from current to parent to grandparent ...
(define ancestry (make-parameter (list)))

; using an opaque struct here naturally protects us from recursing
(struct no-recurse (node))
(struct replace-multi (nodes))
(define (delete-node) (replace-multi (list)))

(struct result (node accum))

(define (return node [accum (nothing)])
  (result node accum))

(define/contract (do-callback callback node)
  (-> callback? node? node?)
  (if (not callback)
      node
      (let* ([result (match (procedure-arity callback)
                       [1 (callback node)]
                       [2 (callback node (accumulator))]
                       [3 (callback node (accumulator) (ancestry))])]
             [result (if (result? result)
                         result
                         (error "TODO callback must return a {result?}"))]
             [accum (result-accum result)]
             [node (if (nothing? (result-node result))
                       node
                       (result-node result))])
        ; if a new accumulator was returned, set it
        (if (not (nothing? accum))
            (accumulator accum)
            (void))
        node)))

(define/contract (get-members node stype)
  (-> struct? struct-type? list?)
  (let-values ([(_1 _2 _3 accessor _5 indexes base-stype _8)
                (struct-type-info stype)])
    (let ([my-members (map (curry accessor node) indexes)])
      (if base-stype
          (append (get-members node base-stype) my-members)
          my-members))))

(define/contract (recompose stype members)
  (-> struct-type? list? struct?)
  (define ctor (struct-type-make-constructor stype))
  (apply ctor members))

(define/contract (do-struct node stype)
  (-> struct? struct-type? struct?)
  (let* ([members (get-members node stype)]
         [members (do-list members)])
    (recompose stype members)))

; A flattening function that only looks for instance of replace-multi
(define/contract (custom-flatten nodes)
  (-> list? list?)
  (match nodes
    ['() '()]
    [(list (replace-multi head) rest ...)
     (append head (custom-flatten rest))]
    [(list head rest ...)
     (cons head (custom-flatten rest))]))

; Perhaps we should only do custom-flatten for lists that aren't struct members?
(define/contract (do-list nodes)
  (-> list? list?)
  (custom-flatten (map visit nodes)))

(define/contract (stype node)
  (-> any/c (or/c #f struct-type?))
  (let-values ([(x _) (struct-info node)])
    x))

(define (visit node #:skip [skip #f])
  (parameterize ([ancestry (cons node (ancestry))])
    (let* (; do the enter callback
           [node
            (do-callback (and (not skip) (enter-callback)) node)]
           ; recurse into children, if applicable
           [node
            (let ([stype (stype node)])
              (cond
                [stype (do-struct node stype)]
                [(list? node) (do-list node)]
                [else node]))]
           ; do the exit callback
           [node
            (do-callback (and (not skip) (exit-callback)) node)]
           ; if a callback wrapped the node in a no-recurse, unwrap it now.
           [node (if (no-recurse? node)
                     (no-recurse-node node)
                     node)])
      node)))

(define/contract (walk root
                       #:accum [accum (nothing)]
                       #:on-enter [on-enter #f]
                       #:on-exit [on-exit #f]
                       #:return [return #f]
                       #:skip-root [skip-root #f])
  (->* (node?)
       (#:accum any/c
        #:on-enter callback?
        #:on-exit callback?
        #:return (or/c 'accum 'root #f)
        #:skip-root any/c)
       any/c)
  (parameterize
      ([accumulator accum]
       [enter-callback on-enter]
       [exit-callback on-exit])
    (let ([result (visit root #:skip skip-root)]
          [accum-val (accumulator)])
      (cond
        [(equal? return 'root) result]
        [(equal? return 'accum) accum-val]
        [(nothing? accum-val) result]
        [else (cons accum-val result)]))))


(module+ test
  (define (callback node accum)
    (match node
      ; When we find a string, append the accumulator to the end. Increment accumulator.
      [str #:when (string? str)
           (return (format "~a~a" str accum)
                   (add1 accum))]
      ; When we find a list starting with 'Counted, insert the accumulator to the right. Increment accumulator.
      [(list 'Counted rest ...)
       (return (cons 'Counted (cons accum rest))
               (add1 accum))]
      ; When we find a list starting with 'Delete, delete it
      [(list 'Delete rest ...)
       (return (delete-node))]
      ; When we find a list starting with 'Duplicate, duplicate it
      [(list 'Duplicate rest ...)
       (return (replace-multi (list node node)))]
      [else (return node)]))

  (check-equal?
   (walk '(1 2 ("a" (3 "b"))) #:on-enter callback #:accum 42)
   (cons 44 '(1 2 ("a42" (3 "b43")))))
  ; Test that it works with prefab structs
  (struct pf (a b c) #:prefab)
  (check-equal?
   (walk (pf 100 "prefab" '(1 2 ("a" (3 "b")))) #:on-enter callback #:accum 42)
   (cons 45 (pf 100 "prefab42" '(1 2 ("a43" (3 "b44"))))))

  (check-equal?
   (walk '(1 2 (Counted a b (Counted c d))) #:on-enter callback #:accum 42)
   (cons 44 '(1 2 (Counted 42 a b (Counted 43 c d)))))
  (check-equal?
   (walk (pf 100 "prefab" '(1 2 (Counted a b (Counted c d)))) #:on-enter callback #:accum 42)
   (cons 45 (pf 100 "prefab42" '(1 2 (Counted 43 a b (Counted 44 c d))))))

  ; Same test as above, but with on-exit instead
  (check-equal?
   (walk '(1 2 (Counted a b (Counted c d))) #:on-exit callback #:accum 42)
   (cons 44 '(1 2 (Counted 43 a b (Counted 42 c d)))))
  (check-equal?
   (walk (pf 100 "prefab" `(1 2 (Counted a ,(pf 200 "prefab" 200) (Counted c d))))
         #:on-exit callback #:accum 42)
   (cons 46 (pf 100 "prefab42" `(1 2 (Counted 45 a ,(pf 200 "prefab43" 200) (Counted 44 c d))))))

  ; In this case we delete right away and don't encounter the "a" and "b" so the accumulator remains 42
  (check-equal?
   (walk '(1 2 (Delete (3 "a") "b") 4) #:on-enter callback #:accum 42)
   (cons 42 '(1 2 4)))
  ; In this case, we encounter the "a" and "b" before we delete, so the accumulator increases
  (check-equal?
   (walk '(1 2 (Delete (3 "a") "b") 4) #:on-exit callback #:accum 42)
   (cons 44 '(1 2 4)))

  (check-equal?
   (walk '(1 2 (Duplicate 3 4) 5) #:on-enter callback)
   '(1 2 (Duplicate 3 4) (Duplicate 3 4) 5))

  ; Using on-exit, first we see "a" and "b" then we see (Duplicate "a42" "b43")
  (check-equal?
   (walk '(1 2 (Duplicate "a" "b") 5) #:on-exit callback #:accum 42)
   (cons 44 '(1 2 (Duplicate "a42" "b43") (Duplicate "a42" "b43") 5)))

  ; This is kind of tricky but I think it makes the most sense:
  ; the walker does not recurse over the contents of replace-multi.
  ; If we recursed it could easily cause infinite recursion.
  ; For example our callback matches:
  ;    (list 'Duplicate x ...)
  ; and returns:
  ;    (replace-multi (list (list 'Duplicate x ...) (list 'Duplicate x ...)))
  ; If the walker were to recurse over the contents of replace-multi we would keep
  ; doubling the number of duplicates until we run out of memory.
  ; So I think it is better to not automatically recurse. The callback can easily
  ; add explicit recursion over some or all of the contents if it wants to.
  (check-equal?
   (walk '(1 2 (Duplicate "a" "b") 5) #:on-enter callback #:accum 42)
   (cons 42 '(1 2 (Duplicate "a" "b") (Duplicate "a" "b") 5)))

  ; Now test the ancestry.
  ; We will create this struct to mean "add the given value to integers in the current list and below"
  (struct adder (val) #:transparent)

  ; Walks up the ancestry to find the first adder, or 0 for none.
  ; (Let's only consider adders in the first position of a list.)
  (define (find-adder ancestry)
    (match ancestry
      [(list (list (adder val) rest1 ...) rest2 ...) val]
      [(list first rest ...) (find-adder rest)]
      [else 0]))

  (define (callback2 node accum ancestry)
    (cond [(adder? node)
           (return (no-recurse node))]
          [(integer? node)
           (let* ([adder (find-adder ancestry)]
                  [new-node (+ node adder)])
             (return new-node (+ accum new-node)))]
          [else (return node)]))

  (check-equal?
   (walk `(1 2 3 (,(adder 100) 4 (5 "x" (,(adder 200) 6)) 7) 8)
         #:on-enter callback2 #:accum 0)
   (let ([total (+ 1 2 3 104 105 206 107 8)])
     (cons total `(1 2 3 (,(adder 100) 104 (105 "x" (,(adder 200) 206)) 107) 8))))

  (check-equal?
   (walk `(1 2 (,(adder 100) 3 (pf 4 "prefab" '(5 6 "hi"))))
         #:on-enter callback2 #:accum 0)
   (let ([total (+ 1 2 103 104 105 106)])
     (cons total `(1 2 (,(adder 100) 103 (pf 104 "prefab" '(105 106 "hi")))))))

  ; Test structs with supertype
  (struct base (a) #:transparent)
  (struct sub base (b) #:transparent)
  (check-equal?
   (get-members (sub (base 1) 'hi) (stype (sub (base 1) 2)))
   (list (base 1) 'hi)))