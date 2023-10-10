#lang racket

;resolve_env a var_env -> output: 1
(define resolve_env
  (lambda (var_name env)
    (cond
      ((null? env) (print "Error: variable not found"))
      ((eq? #f (resolve_scope var_name (car env)))
       (resolve_env var_name (cdr env)))
      (else
       (resolve_scope var_name (car env)))
       )
    )
  )

;resolve_scope a (car var_env)) -> output: 1
;scope is a list of pair values
(define resolve_scope
  (lambda (var_name scope)
    (cond
      ((null? scope) #f)
      ((eq? var_name (car (car scope)))
       (car (cdr (car scope))))
      (else
       (resolve_scope var_name (cdr scope)))
      )
    )
  )

;push_var_to_env (d 4) -> '((d 4) (a 1) (b 2) (c 3))
(define push_var_to_env
  (lambda (var_name var_value env)
    (cons
     (list (list var_name var_value))
     env
     )
   )
  )

(provide (all-defined-out))