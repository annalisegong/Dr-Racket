#lang racket

(define merge (lambda (lst1 lst2)
                (cond
                  ((and (null? lst1) (null? lst2) '()))
                  ((null? lst1) lst2)
                  ((null? lst2) lst1)
                  ((< (car lst1) (car lst2)) (cons (car lst1) (merge (cdr lst1) lst2)))
                  (else (cons (car lst2) (merge lst1 (cdr lst2))))
                                   )))

; sort - rerturns a list of elements of list of integers in ascending order
(define sort (lambda (lst) (
                   if (null? lst) '()
                      (merge (list (car lst)) (sort (cdr lst))))))

(sort '(5 4 3 2 1 9))


(define merge/predicate (lambda (pred lst1 lst2)
                (cond
                  ((and (null? lst1) (null? lst2)) '())
                  ((null? lst1) lst2)
                  ((null? lst2) lst1)
                  ((pred (car lst1) (car lst2)) (cons (car lst1) (merge/predicate pred (cdr lst1) lst2)))
                  (else (cons (car lst2) (merge/predicate pred lst1 (cdr lst2))))
                                   )))
;(merge/predicate < '(1 3 8) '(2 3 4 9))
;(merge/predicate >'(8 3 1) '(9 4 3 2))


; sort predicate - returns a list of element sorted by the predicate
(define sort/predicate (lambda (pred lst)
                         (if (null? lst) '()
                             (merge/predicate pred (list (car lst)) (sort/predicate pred (cdr lst))))))

(sort/predicate > '(1 7 3 8 4 5))
(sort/predicate < '(1 3 8 4 5 7))


