#lang reader "lang-test.rkt"

; Example:
#;{from x "X"
        {select x.Foo {.cast varchar} + "hi" #:as "blah"}}

; Use pipes to avoid the custom read of "."
(define |a . b| "hi")

(add1 10)
(if #t (* 3 9) #f)

(if {equal? {10.add1} 11} 'yes 'no)

{10}

{(list 1 2 3) .length .add1}

(procedure-arity identity)

; These correctly error:
;{identity .procedure-arity}
;{(identity identity) .procedure-arity}
;{.add1 10}

; TODO need to handle this one:
;{.add1}

{if 10.add1 'ok 'nope}

{begin {begin {begin 10.add1}}}

{if 41 .add1 (.* 2) (.equal? 84) "yep, 84" "not 84"}

; TODO start adding infix stuff
;{if {41.add1 * 2 = 84} "it sure does" "what?"}
;{if {41.add1 * 2 equal? 84} 8844 8855}