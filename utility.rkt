#lang racket

;resolve_env a var_env -> output: 1
(define resolve_env
  (lambda (var_name env)
    (cond
      ((null? env) (error-output "Variable not found"))
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

;push_vars_to_env (x y z) (1 2 3) -> '((x 1) (y 2) (z 3)) env
(define push_vars_to_env
  (lambda (list_var list_value env)
    (cons (pair_helper list_var list_value) env)
   )
  )

;truncate env scopes until only global scope is left
;not perfect when env is empty or env is not an env
(define trim_to_global_scope
  (lambda (env)
    (cond
      ((not (pair? env)) (error-output "Illegal env passed into trim function"))
      ((eq? 1 (length env)) env)
      (else (trim_to_global_scope (cdr env)))
     )
    )
  )

;pair_helper (x) (1) -> (x 1)
(define pair_helper
  (lambda (list_var list_value)
    (if
     (null? list_var) '()
     (cons (list (car list_var) (car list_value)) (pair_helper (cdr list_var) (cdr list_value)))
     )
    )
  )

;iterate through each item in list until true or false returned
(define is_in_list
  (lambda (lst item)
    (cond
      ((null? lst) #f) ;if list is empty, return false
      ((eq? (car lst) item) #t) ;if first item in lst == item return true
      (else (is_in_list (cdr lst) item)) ;else, check remainder of list
      )
    )
  )

;display error message with corresponding error from output
(define error-output
  (lambda (output)
    (displayln (string-append "*** error *** " output))
    )
  )

;remove #void from display in 'out processor
(define pick_first_non_void_from_list
  (lambda (lst)
    (cond
      ((not (pair? lst)) (error-output "received invalid parameter for a list"))
      ((and (pair? lst) (eq? (length lst) 1) (void? (car lst))) (display ""))
      ((void? (car lst)) (pick_first_non_void_from_list (cdr lst)))
      (else (car lst))
   )
  )
)

(provide (all-defined-out))