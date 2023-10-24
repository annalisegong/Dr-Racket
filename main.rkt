#lang racket
(require "utility.rkt")
(require "parser.rkt")
(require "processor.rkt")


(define var_env
  '(;environment
   (;global variable scope
    (a 1) (b 3) (c 5)
   )))

(define parsed
  '(call (function (x y) (* x y)) (5 3))
  )

(parser parsed)

(processor (parser '(call (function (x y) (* x y)) (b c))) var_env)


;'(call (function (x y) (* x y)) (5)))

;(processor (parser parsed) var_env)

;(define local (push_vars_to_env '(l m) '(5 6) var_env))

;(add_vars_to_top_scope '(x y z) '(99 100 101) local)

;parsed -> (app-exp (func-exp ((var-exp x) (var-exp y)) (math-exp * (var-exp x) (var-exp y))) ((num-exp 5))