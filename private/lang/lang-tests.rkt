#lang reader "reader.rkt"
(require rackunit)

; Use pipes to avoid the custom read of "."
(define |a.b| "hi")
(check-equal? |a.b| "hi")

(check-true {equal? 10.add1 11})

(check-equal? {42} 42)

(check-equal? {(list 1 2 3).length.add1} 4)
{check-equal? '(1 2 3).length.add1 4}

; These correctly error. How to test?
;{identity .procedure-arity}
;{(identity identity) .procedure-arity}
;{.add1 10}
;{.add1}

(check-equal? {begin {begin {begin 10.add1}}} 11)
(check-equal? (begin (begin {begin 10.add1})) 11)

(check-true {41.add1(.* 2)(.equal? 84)})
(check-true {41 .add1 (.* 2) (.equal? 84)})

(check-true {if 41.add1(.* 2)(.equal? 84) #t #f})
(check-true {if 41 .add1 (.* 2) (.equal? 84) #t #f})

{check-true 41.add1 * 2 = 84}
(check-true {if 41.add1 * 2 = 84 #t #f})

; OK, this is an error but the message is bad.
; (To be fair, if you didn't put a space before the dot it would be way more obvious that it's wrong)
;{10 .add1 * 9 .println}

{check-equal? {10.add1 * 9}.~a "99"}

{define {foo x} x + 10 * 10}
(check-equal? {foo 4} 104)