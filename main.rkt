#lang racket
(require "utility.rkt")
(require "parser.rkt")
(require "processor.rkt")


(define var_env
  '(;environment
   (;global variable scope
    (a 1) (b 3) (c 5)
   )))

(define code
  '(call (function (a) (call (function (r) a) (a))) (5))
)
(define parsed (parser code))

(processor parsed var_env)