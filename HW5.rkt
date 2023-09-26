#lang racket
;bintree? Bintree = int or (symbol Bintree Bintree) this function checks if list is bintree

(define Bintree? (lambda (tree)
  (cond
    ((number? tree) #t)
    ((and (list? tree) (eq? (length tree) 3) (symbol? (car tree)) (Bintree? (car (cdr tree))) (Bintree? (car (cdr (cdr tree))))) #t)
    (else #f)
   )
  )
 )

(Bintree? 1) ;true
(Bintree? '(a 1 2)) ;true
(Bintree? 'a) ;false
(Bintree? '(e 1)) ;false
(Bintree? '(f 1 2 3)) ; false


