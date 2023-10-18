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
  '(call (function(x)(ask (== a 1) (+ x 1) (- x 1))) (2))
  )

;'(call (function (x y) (* x y)) (5)))
 ; '(call (function(x)(ask (== a 1) (+ x 1) (- x 1))) 2))
(processor (parser parsed) var_env)

;(processor (parser parsed) var_env))

;(call (function (x y) (* x y) (5))
;parsed -> (app-exp (func-exp ((var-exp x) (var-exp y)) (math-exp * (var-exp x) (var-exp y))) ((num-exp 5))