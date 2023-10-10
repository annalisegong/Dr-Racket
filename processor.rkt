#lang racket
(require "utility.rkt")

;(processor (var-exp a)) -> (resolve a variable_env) -> 1
(define process_var_exp
  (lambda (parsedCode env)
    (resolve_env (cadr parsedCode) env)
    )
  )

;(processor (app-exp (func-exp ((var-exp x)) (var-exp x)) (var-exp a)))
;function func(x) (...); func(1)
(define process_app_exp
  (lambda
      (parsedCode env)
    (let
        (
         (local_env
          (push_var_to_env
           (cadr (car (cadr (cadr parsedCode))))
           (processor (caddr parsedCode) env)
           env)
          )
         )
      (processor (caddr (cadr parsedCode)) local_env)
    )
  )
 )

;(num-exp 1) -> 1
(define process_num_exp
  (lambda (parsedCode env)
    (cadr parsedCode)
    )
  )

;processor finds the variable, then bounds the identifier to the resolved variable value
;processor (var-exp a)) -> (resolve a variable_env -> 1
;processor (app-exp (func-exp ((var-exp x)) (var-exp x)) (var-exp xa))
(define processor
  (lambda (parsedCode env)
    (cond
      ;when parsedCode is empty
      ((null? parsedCode)
       (displayln "Error: processor received illegal parsed code"))
      ;when parsedCode is a var exp
      ((eq? 'var-exp (car parsedCode))
       (process_var_exp parsedCode env))
      ;when parsedCode is a app exp
      ((eq? 'app-exp (car parsedCode))
       (process_app_exp parsedCode env))
      ;when parsedCode is numeric exp
      ((eq? 'num-exp (car parsedCode))
       (process_num_exp parsedCode env))
      ;when parsedCode is params
      ((eq? 'params (car parsedCode))
       (process_var_exp parsedCode env))
      ;when parsedCode is body expression
      ((eq? 'body-exp (car parsedCode))
       (process_var_exp parsedCode env))
      ;when parsedCode is boolean exp
      
      ;otherwise
      (else #f)
    )
  )
)

(provide (all-defined-out))