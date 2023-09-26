#lang racket
(define variable_env '((a 1) (b 2) (c 33)))
(define resolve (lambda (variable_name variable_env)
    (cond
      ((null? variable_env) (print "error: no element in variable environment"))
      ((eq? (car (car variable_env)) variable_name) (car (cdr (car variable_env))))
      (else (resolve variable_name (cdr variable_env)))
      )
    )
)

(resolve 'a variable_env)
(resolve ''a variable_env)

;(parser 'a) -> var-exp a
;(pasrser '(function (x) x) -> (func-exp ((var-exp x)) (var-exp x)))
;(parser (call (function (x) x) a)) -> (app-exp ((func-exp (var-exp x)) (var-exp x)) (var-exp a))
(define parser (lambda (statement)
                      (cond
                       ((symbol? statement) (list 'var-exp statement))
                       ((and
                         (list? statement)
                         (eq? 'function (car statement))
                         (eq? (length statement) 3)
                         (list 'func-exp (list (parser (car (cadr statement))) (parser (car (caddr statement)))))
                         ;this if a function expression
                       ))
                       ((and
                         (list? statement)
                         (eq? 'call (car statement))
                         (eq? (length statement) 3)
                         (list 'app-exp (parser (cadr statement)) (parser (caddr statement)))
                         ;this is a app expression
                         ))
                       (print "parsing failed. unkown statement.")
                       )
                      )
  )
(parser '(function (x) x))

;processor (var-exp a)) -> (resolve a variable_env -> 1
(define processor (lambda (parse variable_env)
                    (cond
                      ((null? parse) (print "processing failed. bad parsing syntax"))
                      ((eq? (car parse) 'var-exp) resolve (cadr parse) variable_env))
                      (print "processing failed. unknown issue")
                    )
  )
(processor (parser 'a) variable_env)

(define executor (lambda (code)
                   (processor (parser code) variable_env)
                   ))
(executor 'a)
          
                                      