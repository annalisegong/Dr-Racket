#lang racket
(define product-helper (lambda (s sos)
                         (
                          if (null? sos) '()
                             (cons (list s (car sos)) (product-helper s (cdr sos)))
                             ))
  )

(define product (lambda (sos1 sos2)
  (
   if (null? sos1) '()
      (append (product-helper (car sos1) sos2) (product (cdr sos1) sos2))
            )
      )
  )
(product-helper 'a '(x y))

(define product_map (lambda (sos1 sos2)
  (
   if (null? sos1) '()
      (append (map (lambda (s) (list (car sos1) s)) sos2) (product_map (cdr sos1) sos2))
            )
      ))

(product '(a b c) '(x y))
(product_map '(a b c) '(x y))

(define product1 (lambda (sos1 sos2)
                  (map (lambda (s3) (car s3)) (
        map (lambda (s) (map (lambda (s2) (
                                      list s s2)
                                          ) sos2)) sos1
))))

;(filter-in pred lst) - goes through list and returns elements that match pred 
;(filter-in number? '(1 a b 2 3 "abd" d)) -> '(1 2 3)
  (define filter-in (lambda (pred lst) (
                cond
                 ((null? lst) '())
                 ((pred (car lst)) (cons (car lst) (filter-in pred (cdr list))))
                 (else (filter-in pred (cdr lst)))
                                        )))
(filter-in number? '(1 a b 2 3 "abc" d 4))

(define list-index-helper (lambda (index pred lst) (
        if (null? lst) #f
           (if (pred (car lst)) index
               (list-index-helper (+ 1 index) pred (cdr lst))
           ))))
       

;(list-index number? '(a b 1 c (a b)) -> 2
(define a 0)
(define list-index (lambda (pred lst) (
                      if (null? lst) #f
                        (list-index-helper 0 pred lst)
                      )))

(list-index number? '(a v s 2))