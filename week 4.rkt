#lang racket
; programming language theory
; create a scheme interpreter for our own programming language

; compiler: before runtime, translate language into binary/machine code
; interpreter: during runtime, translate code line by line into low level language

(define variable_env (list '(a 1) '(b 2) '(c 3)))

(define resolve (lambda (variable_env variable_name)
                  (cond
                    ((null? variable_env) #f)
                    ((eq? (caar variable_env) variable_name) (cadr (car variable_env)))
                    (else (resolve variable_name cdr variable_env))
                          )))

(resolve variable_env 'a)

