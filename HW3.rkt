#lang racket

;flatten lst - returns list of symbols contained in lst in the orfer in which they occur
; and removes all inner parentheses from its argument
(define flatten (lambda (lst) (
                     ; if null, return empty list
                     cond ((null? lst) '())
                       ; if first element in list is a list, flatten first element and flatten rest of list, then append together
                       ((list? (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
                       ; if element in list is not part of a list, add first element and flatten rest of list
                       (else (cons (car lst) (flatten (cdr lst)))))
                       ))

(flatten '(a b (c d) ((e (f)))))

;merge - sorts two lists of integers already sorted into ascending order are merged together into one list in asc order
(define merge (lambda (lst1 lst2)
                (cond
                  ; if lst1 and lst2 are both empty, return empty list
                  ((and (null? lst1) (null? lst2) '()))
                  ; if lst1 is empty, add first element of lst2 then keep adding first element of remaining in lst2
                  ((null? lst1) (cons (car lst2) (merge lst1 (cdr lst2))))
                  ; if lst2 is empty, add first element of lst1 then keep adding first element of remaining in lst1
                  ((null? lst2) (cons (car lst1) (merge (cdr lst1) lst2)))
                  ; if first element in lst1 is > first element in lst2, add first element lst2 and check lst1 w remainder lst2
                  ((> (car lst1) (car lst2)) (cons (car lst2) (merge lst1 (cdr lst2))))
                  ; if first elment in lst2 is > lst1, or elements are even, add first element lst1 and check remainder lst1 w lst2
                  (else (cons (car lst1) (merge (cdr lst1) lst2)))
                                   )))
(merge '(1 2 4 53 66) '(3 12 33 91))

