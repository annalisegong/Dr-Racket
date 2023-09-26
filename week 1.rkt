#lang racket
(define a 1) ;create a variable the identifier is a, and assign value 1 to a

(define lst
  '(1 2 3)
  ) ;statement starts with a function name and is followed by a symbol/variable name,
; then a value for the variable

(car lst) ;returns the first element in the list
(car '("a" 22 23)) ; this will return a
(car '('(1 2 3) 22 23)) ; this will return the list (1 2 3)
(car '('() 22 23)) ; this will return an empty list ()

(cdr lst) ;this returns a new list with all elements from lst except the first
(cdr '(1)) ;this will return an empty list ()
(cadr lst) ; is the same as (car (cdr list))
(cddr lst) ; is the same as (cdr (cdr list))

(define lst2 '('a 'b 'c 'd))
(car (cdr (cdr lst2))) ; this returns the third element

;predicates
(null? lst) ;lst is NULL // lst === null; this is a boolean expression/statement
(equal? lst 1) ;lst === 1?

(define b a)
(define c "a")
(equal? b c) ;returns false because b is the value of variable a c is the character a

;conditionals
(if (list? lst) "yes" "no") ; this will return "yes"

;lambda
(define lst '(1 2 3))
(lamba (x) (+ x x)) ; returns 4
(define square (lamba (x) (* x x)))
(square 4) ;returns 16