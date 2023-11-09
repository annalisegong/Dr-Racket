#lang racket
(require "utility.rkt")
(require "parser.rkt")
(require "processor.rkt")


(define var_env
  '(;environment
   (;global variable scope
    (a 1) (b 2) (c 3)
   )))

(define code
  ;'(block (assign x 10) (out x))
  '(while (< a 4) ((let (a (+ a 1)) (out a))))
)

(processor (parser '(out a)) var_env)
(println "")

(define parsed (parser code))
 parsed
(processor parsed var_env)