#lang racket
(require "simpleParser.rkt")

;===============================================================
;; Interpreter 
;; Tyler Powers, Henry Odza, and Benjamin Zheng 
;;
;; CSDS 345
;=======================================

; Calls the parser on the input file 
; Input: input file with code
(define interpret (lambda (input) (parser input)))

; The state of the interpreter. Starts empty
; Format (var_list val_list)
(define state '(() ())) 

(define append_state 
  (lambda (var val old_state)
    (cons (append_var var (car old_state))
          (append_val val (cdr old_state)))))

; Appends a value to the value list within state
(define append_var
  (lambda (var var_list)
    (cons var var_list)))

; Appends a variable to the variable list within state
(define append_val
  (lambda (val val_list)
    (cons val val_list)))

 