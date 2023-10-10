#lang racket
; Parser - changes the input code into a format that is easier to process
;(parser 'a) -> var-exp a
;(pasrser '(function (x) x) -> (func-exp ((var-exp x)) (var-exp x)))
;(parser (call (function (x) x) a)) -> (app-exp ((func-exp (var-exp x)) (var-exp x)) (var-exp a))

(define params
  (lambda (statement)
    (cond
      ((symbol? statement) (list 'params statement))
      (print "unknown statement")
      )))
(define body
  (lambda (statement)
    (cond
      ((symbol? statement) (list 'body-exp statement))
      (print "unknown statement")
      )))

(define parser
  (lambda (statement)
    (cond
      ((symbol? statement) (list 'var-exp statement)) ;this is a variable expression returns 'var-exp
      ((number? statement) (list 'num-exp statement)) ;this is a numeric expression returns 'num-exp
      ((and
       (list? statement) ; if statement is a list of items and 
       (eq? 'function (car statement)) ;if first item in list is == 'function and
       (eq? (length statement) 3)) ; if length of statement list == 3
       (list 'func-exp (list (params (car (cadr statement)))) (body (caddr statement)))) ;then this if a function expression
      ; so it returns 'func-exp and continues parsing 2nd item then 3rd item in statement list
      ((and
       (list? statement)
       (eq? 'call (car statement)) ;if first item in list == 'call and
       (eq? (length statement) 3)) ; if length of statement list == 3
       (list 'app-exp (parser (cadr statement)) (parser (caddr statement))) ;then this is an app-exp
       ;so it returns 'app-exp and continues parsing with 2nd item then 3rd item in statement list
      )
      ;else return error message
       (print "parsing failed. unkown statement.")
    )
  )
)

(provide (all-defined-out))