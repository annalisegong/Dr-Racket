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
          (push_vars_to_env
           (map (lambda (arg) (cadr arg)) (cdr (car (cadr (cadr parsedCode)))))
           (map (lambda (val-exp) (processor val-exp env)) (cdr (caddr parsedCode)))
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

;bool-exp == (var-exp x) (num-exp 1) -> (operator x 1) -> #t or #f
(define process_bool_exp
  (lambda (parsedCode env)
    (cond
      ((eq? '< (cadr parsedCode))
       (< (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
      ((eq? '> (cadr parsedCode))
       (> (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
      ((eq? '<= (cadr parsedCode))
       (<= (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
      ((eq? '>= (cadr parsedCode))
       (>= (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
      ((eq? '== (cadr parsedCode))
       (eq? (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
      ((eq? '&& (cadr parsedCode))
       (and (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
      ((eq? '|| (cadr parsedCode))
       (or (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
      ((eq? '! (cadr parsedCode))
       (not (processor (caddr parsedCode) env)))
      ((eq? '!= (cadr parsedCode))
       (not (eq? (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env))))
      (else (println "Error: bad boolean expression"))
      )
    )
  )

;ask ( == x 1) true-exp false-exp -> (if == x 1) -> true-exp or false-exp
(define process_ask_exp
  (lambda (parsedCode env)
    (if
     (processor (cadr parsedCode) env)
     (processor (caddr parsedCode) env)
     (processor (cadddr parsedCode) env))
    )
  )

;math
(define process_math_exp
  (lambda (parsedCode env)
    (cond
      ((eq? '+ (cadr parsedCode))
       (+ (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
       ((eq? '- (cadr parsedCode))
       (- (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
       ((eq? '* (cadr parsedCode))
       (* (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
       ((eq? '/ (cadr parsedCode))
       (quotient (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
       ((eq? '// (cadr parsedCode))
       (/ (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
       ((eq? '% (cadr parsedCode))
       (modulo (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
       (else (println "Error: bad math expression"))
     )
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
      ;when parsedCode is boolean exp
      ((eq? 'bool-exp (car parsedCode))
       (process_bool_exp parsedCode env))
      ;when parsedCode is asked expression
      ((eq? 'ask-exp (car parsedCode))
       (process_ask_exp parsedCode env))
      ;when parsedCode is math expression
      ((eq? 'math-exp (car parsedCode))
       (process_math_exp parsedCode env))
      ;otherwise
      (else #f)
    )
  )
)

(provide (all-defined-out))