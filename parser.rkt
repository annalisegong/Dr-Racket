#lang racket
(require "utility.rkt")
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

;check boolean operators
(define check_bool
  (lambda (op)
    (cond
      ((eq? '< op) #t)
      ((eq? '> op) #t)
      ((eq? '<= op) #t)
      ((eq? '>= op) #t)
      ((eq? '== op) #t)
      ((eq? '&& op) #t)
      ((eq? '|| op) #t)
      ((eq? '! op) #t)
      ((eq? '!= op) #t)
      (else #f)
      )
    )
  )

;check math operators
(define check_math
  (lambda (op)
    (is_in_list (list '+ '- '/ '// '%) op)
    )
  )

(define parser
  (lambda (statement)
    (cond
      ((symbol? statement) (list 'var-exp statement)) ;this is a variable expression returns 'var-exp
      ((number? statement) (list 'num-exp statement)) ;this is a numeric expression returns 'num-exp
      ;if list with 'func-exp
      ((and
       (list? statement) ; if statement is a list of items and 
       (eq? 'function (car statement)) ;if first item in list is == 'function and
       (eq? (length statement) 3)) ; if length of statement list == 3
       (list 'func-exp (list (parser (car (cadr statement)))) (parser (caddr statement)))) ;then this if a function expression so it returns 'func-exp and continues parsing 2nd item then 3rd item in statement list
      ;if list with 'app-exp
      ((and
       (list? statement)
       (eq? 'call (car statement)) ;if first item in list == 'call and
       (eq? (length statement) 3)) ; if length of statement list == 3
       (list 'app-exp (parser (cadr statement)) (parser (caddr statement))) ;then this is an app-exp so it returns 'app-exp and continues parsing with 2nd item then 3rd item in statement list
      )
      ;if list with 'bool-exp
      ((and
        (list? statement) ;if statement is a list of items and 
        (check_bool (car statement)) ;if first item in the list == any of bool operators in check_bool
        (eq? (length statement) 3)) ;if length of statement == 3
        (list 'bool-exp (car statement) (parser (cadr statement)) (parser (caddr statement)))
        )
      ;if list with ! 
      ((and
        (list? statement) ;if statement is a list of items and 
        (eq? '! (car statement)) ;if first item in the list == ! and
        (eq? (length statement) 3)) ;if length of statement == 3
        (list 'bool-exp (car statement) parser (cadr statement)) ;then this is a bool-exp ! continue parse on 2nd item
        )
      ;if it's an "if" statement
      ((and
        (list? statement) ;if statement is a list of items and 
        (eq? 'ask (car statement)) ;if first item in the list == 'ask
        (eq? (length statement) 4)) ;if length of statement == 3
        (list 'ask-exp (parser (cadr statement)) (parser (caddr statement)) (parser (cadddr statement))) ;then it's an if/ask expression so continue parsing 2nd, 3rd, 4th items
        )
      ;if math expression
      ((and
        (list? statement)
        (check_math (car statement))
        (list 'math-exp (car statement) (parser (cadr statement)) (parser (caddr statement))))
        )
      ;else return error message
       (print "parsing failed. unkown statement.")
    )
  )
)

(provide (all-defined-out))