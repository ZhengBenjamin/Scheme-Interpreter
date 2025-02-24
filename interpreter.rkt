#lang racket
(require "simpleParser.rkt")
(provide (all-defined-out))
;=======================================
;; Interpreter 
;; Tyler Powers, Henry Odza, and Benjamin Zheng 
;;
;; CSDS 345
;=======================================

; Calls the parser on the input file 
; Input: input file with code
(define interpret (lambda (input) (parser input)))

; Declares a new variable with no value 
; Returns state with var added to it
; TODO: Check if var already exists
(define var_dec
  (lambda (var state)
    (if var_exists? var state)
      (error "Variable already exists")
      (append_state var null state)))

; Declares a new variable with a value 
(define var_dec_assn
  (lambda (var val state) 
    (if var_exists? var state)
      (error "Variable already exists")
      (append_state var val state)))


;=======================================
;; State Logic 
;=======================================

; The state of the interpreter. Starts empty
; Format (var_list val_list)
(define init_state '(() ()))

; Calls append_var and append_val to map val to var within state
(define append_state 
  (lambda (var val old_state)
    (cons (append_var var (car old_state))
          (list (append_val val (car (cdr old_state)))))))

; Appends a value to the value list within state
(define append_var
  (lambda (var var_list)
    (cons var var_list)))

; Appends a variable to the variable list within state
(define append_val
  (lambda (val val_list)
    (cons val val_list)))

