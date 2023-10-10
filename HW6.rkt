#lang racket
(require "utility.rkt")
(require "parser.rkt")
(require "processor.rkt")


(define var_env
  '(;environment
   (;global variable scope
    (a 1) (b 2) (c 3)
   )))

(define parsed
  (parser '(call (function (x) x) 1))
  )

(define execute
  (lambda (code)
    (processor (parser code) var_env)
    )
  )

(parser '(call (function (x) x) a))
;expected output: (app-exp (func-exp (params x) (body-exp x) (var-exp a)))

(execute '(call (function (x) x) a))
;expected output: 1
