#lang racket
; (swapper s1 s2 slist) - returns a list the same as
(define swapper (lambda (s1 s2 slist)(
                   if (null? slist) '()
                       (cond
                         ;first list is equal to first item in slist, add second list to returned values
                         ((equal? s1 (car slist)) (cons s2 (swapper s1 s2 (cdr slist))))
                         ;second list is equal to first item in slist, add first list to returned values
                         ((equal? s2 (car slist)) (cons s1 (swapper s1 s2 (cdr slist))))
                         ;add first item in slist to returned values
                         (else (cons (car slist) (swapper s1 s2 (cdr slist))))
                       ))))
(swapper 'a 'd '(a b c d)) ;returns '(d b c a)

;(list-set lst n x) - returns a list like lst, except that the nth element, using zero-based indexing is x
(define list-set (lambda (lst n x) (
                     if (null? lst) '()
                        (cond
                          ;nth term is longer than index in list, return og list
                          ((< n 0) lst)
                          ;nth term equal to 0 index, add x to front of list
                          ((eq? n 0) (cons x (cdr lst)))
                          ;nth term is not the first/last index, add the first item to returned values, and subtract one from nth term
                          (else (cons (car lst) (list-set (cdr lst) (- n 1) x))) 
                        ))))
(list-set '(a b c d) 2 '(1 2)) ; returns '(a b '(1 2) d)

;(count-occurences s slist) - returns number of occcurences of s element that exist in slist
(define count-occurences (lambda (s slist) (
                             cond
                               ;case - 1: empty or end of list
                               ((null? slist) 0) 
                               ;case - 2: element in list matches s
                               ((eq? (car slist) s) (+ 1 (count-occurences s (cdr slist))))
                               ;case - 3: element in list does not match s and we're not at the end of the list
                               (else (count-occurences s (cdr slist))) 
                             )))
(count-occurences 'a '(a a a b c d a a)) ; returns 5