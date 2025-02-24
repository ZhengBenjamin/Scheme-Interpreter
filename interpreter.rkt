#lang racket
(require "simpleParser.rkt")

;===============================================================
;; Interpreter 
;; Tyler Powers, Henry Ozda, and Benjamin Zheng 
;;
;; CSDS 345
;=======================================

; Calls the parser on the input file 
(define call_parser (lambda (input) (parser input)))
