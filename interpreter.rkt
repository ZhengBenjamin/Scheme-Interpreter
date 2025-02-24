#lang racket
(require "simpleParser.rkt")

;===============================================================
;; Interpreter 
;; Tyler Powers, Henry Ozda, and Benjamin Zheng 
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
  (lambda (var, val)
    (append_var var (car state))
    (append_val val (cdr state))))

; Appends a value to the value list within state
(define append_val
  (lambda (val, val_list)
      (if pair? val_list) 
        (cons (car val_list) (append_val val (cdr val_list)))
        (cons val val_list)))

; Appends a variable to the variable list within state
(define append_var
  (lambda (var, var_list)
    (if pair? var_list) 
      (cons (car var_list) (append_var var (cdr var_list)))
      (cons var var_list)))

