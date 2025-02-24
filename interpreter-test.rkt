#lang racket/base
 
(require rackunit
         "interpreter.rkt")

(check-equal? (interpret "tests/test1.txt") 150 "test1")