#lang racket
; list
(define b 3)
(define a1 '(1 2 3 b)) ;should return '(1 2 3 b) because does not resolve during runtime
(define a2 (list (1 2 3 b))) ; returns '(1 2 3 3) because resolves at runtime

; cons
(define lst1 ‘(1 2 3))
(define lst2 ‘(4 5 6))
(cons “a” lst1) ; returns '(a 1 2 3)

;length
(length lst1) ; returns 4

;list-ref
(define list1 '(1 2 3))
(define list2 '(4 5 6))
(list-ref '(a b c) 1) ; returns b

(define charAt
  (lambda (lst index)
    (if (= index 0) (car lst)
        (charAt (cdr lst) (- index 1))
        )
    )
  )
(charAt list1 1) ; returns 2