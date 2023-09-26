#lang racket

; duple duplicates the rep for a specific number of times
(define duple (lambda (number rep) ; argument list
  (if (equal? number 0) '() ; equal? is like == ; if equal to 0 return empty list
      (cons rep (duple (- number 1) rep)
      )
      )
  )
  )
(duple 2 '(1 2 3 4))

;inverse for two item list '(a b) --> '(b a)
(define invert_helper (lambda (two_item_list)
      (list (cadr two_item_list) (car two_item_list))
      )
  )

(invert_helper '(1 2))

;inverse list
(define invert (lambda (lst)
  (if (null? lst) '() ; if list is empty, return empty list
      (cons (invert_helper (car lst)) (invert (cdr lst)))
      ; cons (2 1) (invert (a b))
      ; cons (2 1) cons (b a)
      )
  )
)
(invert '((a b)(c d)))

;down function to add () around each item
(define down (lambda (lst)
               (if (null? lst) '()
                   (cons (list (car lst)) (down (cdr lst)))
                   )
               )
  )
(down '(a b c))