#lang racket
(define variable_env '(((a 1) (b 2) (c 33) (d "string") (e a))))

(define resolve_scope (lambda (variable_name scope)
    (cond
      ((null? scope) null)
      ((eq? (car (car scope)) variable_name) (car (cdr (car scope))))
      (else (resolve_scope variable_name (cdr scope)))
      )
    )
)

(define resolve (lambda (variable_name)
                  (if
                    (null? variable_env) (print "error: variable not found in env")
                    (if (null? (resolve_scope variable_name (car variable_env)))
                        (resolve variable_name (cdr variable_env))
                        (resolve_scope variable_name (car variable_env))
                    )
                  )
                )
  )

(define append_var (lambda (variable_name variable_value)
                     (set! variable_env
                           (cons
                            (cons (list variable_name variable_value) (car variable_env))
                            (cdr variable_env)
                            )
                           )
                     )
)

;(define push_scope (lambda ()
                     ;(return cons)
                      ;)
  ;)

;(define pop_scope (lambda ()
                    ;(;return cdr)
                     ; )
;  )
; Parser - changes the input code into a format that is easier to process
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

;processor finds the variable, then bounds the identifier to the resolved variable value
;processor (var-exp a)) -> (resolve a variable_env -> 1
(define processor (lambda (parse variable_env)
                    (cond
                      ((null? parse) (print "processing failed. bad parsing syntax"))
                      ((eq? (car parse) 'var-exp) (resolve (cadr parse) variable_env))
                      ((eq? (car parse) 'app-exp) (print "more to do"))
                      (else "processing failed. unknown issue")
                    )
  ))

(define executor (lambda (code)
                   (processor (parser code) variable_env)
                   ))

(print variable_env)
(append_var 'hunter 20)
(print variable_env)                                    