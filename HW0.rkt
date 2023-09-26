#lang racket
(define lst
  '(1 2 3 4 5)
  )

; define declares and assigns variable (define var_name var_value)
; statement syntax (keyword/function name statement_value)

(define list_length (lambda (x)
                      (if (null? x) 0
                          (+ 1 (list_length (cdr x)))
                          )
  )
)

  (list_length lst)

; in recursion, function will go to the end of list and return the result
; to the previous recursion memory space which will then apply the previous
; result until the current result until only the initial function call exists
