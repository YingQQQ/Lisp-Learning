#lang sicp
(#%provide (all-defined))
(#%require "test.rkt")
(define (test a)
  (write a))

(define (test1 a)
  (write (+ 1 a)))