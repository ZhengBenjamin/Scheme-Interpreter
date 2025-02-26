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
(define interpret 
  (lambda (input)
    (M_state (parser input) init_state)))
; (define interpret
;   (lambda (input)
;     (printf "Interpret called with input: ~a\n" input)
;     (printf "Calling parser\n")
;     (displayln (list (parser input)))))
; (define interpret
;   (lambda (input)
;     (printf "Interpret called with input: ~s\n" input)  ; ~s for precise printing
;     (printf "Calling parser\n")
;     (let ((parsed (parser input)))
;       (printf "Raw parsed output: ~s\n" parsed)  ; Preserve structure
;       (displayln parsed))))


;=======================================
;; M_ functions
;; These functions are the main stateful functions that are called by the parser
;=======================================
(define M_state
  (lambda (statement state)
    (printf "M_state called with statement: ~a and state: ~a\n" statement state)
    (cond
      ((null? statement) state)
      ((list? (car statement)) (M_state 
                                (cdr statement) 
                                (M_state (car statement) state)))
      ((eq? (function statement) 'var) (M_declare
                                        statement
                                        state))
      ((eq? (function statement) '=) (M_assign
                                      statement 
                                      state))
      ((eq? (function statement) 'while) (M_while 
                                          (condition statement) 
                                          (body1 statement) 
                                          state))
      ((eq? (function statement) 'if) (M_if 
                                        statement
                                        state))
      ((eq? (function statement) 'return) (M_return 
                                            (cadr statement) 
                                            state))
      (else (error (format "Invalid statement: ~a" statement))))))

; abstraction
(define function car)

; abstraction for declare/assign
(define varexp cdr)
(define varname cadr)
(define varvalue caddr)

(define M_if
  (lambda (statement state)
    (printf "M_if called with statement: ~a, state: ~a\n" statement state)
    (if (M_boolean (condition statement) state)
        (M_state (body1 statement) state)
        (if (> 4 (length statement))
            state
            (M_state (body2 statement) state)))))

(define M_while
  (lambda (while_statement while_body state)
  (printf "M_while called with while_statement: ~a, while_body: ~a, state: ~a\n" while_statement while_body state)
    (if (M_boolean while_statement state)
        (M_while while_statement while_body state)
        state)))

; abstraction for if/while
(define condition cadr)
(define body1 caddr)
(define body2 cadddr)

(define M_declare
  (lambda (statement state)
    (printf "M_declare called with statement: ~a and state: ~a\n" statement state)
    (cond
      ((var_exists? (varname statement) state) (error ("Variable already exists")))
      ((has_value? statement) (var_dec_assn (varname statement) (M_value (varvalue statement) state) state))
      (else (var_dec (varname statement) state)))))

; abstraction for declare. checks if define has a value
(define has_value? (lambda (statement) (if (null? (cddr statement)) #f #t)))

(define M_assign
  (lambda (statement state)
    (printf "M_assign called with statement: ~a and state: ~a\n" statement state)
    (if (var_exists? (varname statement) state)
      (var_assn (varname statement) (M_value (varvalue statement) state) state)
      (error (format "Variable not declared: ~a" (varname statement))))))

; evaluates a mathematical expression
(define M_value
  (lambda (expression state)
    (printf "M_value called with expression: ~a and state: ~a\n" expression state)
    (cond
      ; logical evaluation
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ; var
      ((number? expression) expression)
      ((var? expression) (get_var expression state))
      ((eq? '|| (op expression)) (or (M_boolean (x expression) state) (M_boolean (y expression) state)))
      ((eq? '&& (op expression)) (and (M_boolean (x expression) state) (M_boolean (y expression) state)))
      ((eq? '! (op expression)) (not (M_boolean (x expression) state)))
      ((eq? '== (op expression)) (eq? (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '!= (op expression)) (not (eq? (M_value (x expression) state) (M_value (y expression) state))))
      ((eq? '> (op expression)) (> (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '< (op expression)) (< (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '>= (op expression)) (>= (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '<= (op expression)) (<= (M_value (x expression) state) (M_value (y expression) state)))
      ; mathematical evaluation
      ((eq? '+ (op expression)) (+ (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '- (op expression)) (subtract expression state))
      ((eq? '* (op expression)) (* (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '/ (op expression)) (quotient (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '% (op expression)) (remainder (M_value (x expression) state) (M_value (y expression) state)))

      (else (error "Invalid expression")))))


(define subtract
  (lambda (expression state)
    (printf "subtract called with expression: ~a and state: ~a\n" expression state)
    (cond 
      ((null? (unary expression)) (- (M_value (x expression) state)))
      (else (- (M_value (x expression) state) (M_value (y expression) state))))))

(define var?
  (lambda (x)
    (printf "var? called with x: ~a\n" x)
    (and (and (not (number? x)) (not (list? x))) (and (not (pair? x)) (not (bool_op? x))))))

(define bool_op?
  (lambda (x)
    (printf "bool_op? called with x: ~a\n" x)
    (or (eq? 'false x) (or (eq? 'true x) (or (eq? '|| x) (or (eq? '&& x) (or (eq? '! x) (or (eq? '== x) (or (eq? '!= x) (or (eq? '> x) (or (eq? '< x) (or (eq? '>= x) (eq? '<= x)))))))))))))
; evaluates a boolean expression  ==, !=, <, >, <=. >=
(define M_boolean
  (lambda (expression state)
    (printf "M_boolean called with expression: ~a and state: ~a\n" expression state)
    (cond
      ((var? expression) (get_var expression state))
      ((boolean? expression) expression)
      ((eq? 'true expression) expression)
      ((eq? 'false expression) expression)
      ((eq? '== (op expression)) (eq? (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '!= (op expression)) (not (eq? (M_value (x expression) state) (M_value (y expression) state))))
      ((eq? '> (op expression)) (> (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '< (op expression)) (< (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '>= (op expression)) (>= (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '<= (op expression)) (<= (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '&& (op expression)) (and (M_boolean (x expression) state) (M_boolean (y expression) state)))
      ((eq? '|| (op expression)) (or (M_boolean (x expression) state) (M_boolean (y expression) state)))
      ((eq? '! (op expression)) (not (M_boolean (x expression) state)))
      (else (error "invalid boolean expression"))
      )))



(define op car)
(define x cadr)
(define y caddr)
(define unary cddr)

(define M_return
  (lambda (statement state)
    (printf "M_return called with statement: ~a and state: ~a\n" statement state)
    (M_value statement state)))

;=======================================
;; Variable Logic
;=======================================

; Variable declaration, assigns var to null 
(define var_dec
  (lambda (var state)
    (printf "var_dec called with var: ~a and state: ~a\n" var state)
    (append_state var 'null state)))

; Variable declaration with assignment, assigns var to val
(define var_dec_assn
  (lambda (var val state)
    (printf "var_dec_assn called with var: ~a, val: ~a and state: ~a\n" var val state)
    (append_state var val state)))

(define var_assn
  (lambda (var val state)
    (printf "var_assn called with var: ~a, val: ~a and state: ~a\n" var val state)
    (if (var_exists? var state)
        (add_binding var (M_value val state) (remove_binding var state))
      (error (format "Variable not declared: ~a" var)))))

; TODO: Implement while loop


;=======================================
;; State Logic
;=======================================

; TODO abstraction for all state logic
  
; The state of the interpreter. Starts empty
; Format (var_list val_list)
(define init_state '(() ()))

; Calls append_var and append_val to map val to var within state
(define append_state 
  (lambda (var val old_state)
    (printf "append_state called with var: ~a, val: ~a and old_state: ~a\n" var val old_state)
    (cons (append_var var (car old_state))
          (list (append_val val (car (cdr old_state)))))))

; Appends a value to the value list within state
(define append_var
  (lambda (var var_list)
    (printf "append_var called with var: ~a and var_list: ~a\n" var var_list)
    (cons var var_list)))

; Appends a variable to the variable list within state
(define append_val
  (lambda (val val_list)
    (printf "append_val called with val: ~a and val_list: ~a\n" val val_list)
    (cons val val_list)))

; Checks if a variable exists in the state
; Returns true if it exists, false otherwise
(define var_exists? 
  (lambda (var state)
    (printf "var_exists? called with var: ~a and state: ~a\n" var state)
    (cond 
      ((null? (car state)) #f)
      ((equal? var (car (car state))) #t)
      (else (var_exists? var (cons (cdr (car state)) (cdr state)))))))

; Checks if a variable has been initialized with a value
; Returns true if it has been initialized, false otherwise
(define var_init?
  (lambda (var state)
    (printf "var_init? called with var: ~a and state: ~a\n" var state)
    (cond
      ((eq? #f (var_exists? var state)) #f)
      ((null? (cdr state)) #f)
      ((equal? var (car (car state))) (not (null? (car (cdr state)))))
      (else (var_init? var (cons (cdr (car state)) (list (cdr (car (cdr state))))))))))

; Sets binding of var to val in the state
(define remove_binding
  (lambda (var state)
    (printf "remove_binding called with var: ~a and state: ~a\n" var state)
    (list (car state) (find_replace_val var (car state) (car (cdr state))))))

; Helper for remove_binding, finds var and replace its val with null
(define find_replace_val
  (lambda (var var_list val_list)
    (printf "find_replace_val called with var: ~a, var_list: ~a, val_list: ~a\n" var var_list val_list)
    (cond
      ((null? var_list) val_list)
      ((equal? var (car var_list)) (cons null (cdr val_list))) ; Set binding to null
      (else (cons (car val_list) (find_replace_val var (cdr var_list) (cdr val_list)))))))

; Sets binding of var to val in the state
(define add_binding
  (lambda (var val state)
    (printf "add_binding called with var: ~a, val: ~a, state: ~a\n" var val state)
    (list (car state) (find_set_val var val (car state) (car (cdr state))))))

; Helper for add_binding, finds var and replace its val with new val
(define find_set_val
  (lambda (var val var_list val_list)
    (printf "find_set_val called with var: ~a, val: ~a, var_list: ~a, val_list: ~a\n" var val var_list val_list)
    (cond
      ((null? var_list) val_list) ;
      ((equal? var (car var_list)) (cons val (cdr val_list))) ; Update binding
      (else (cons (car val_list) (find_set_val var val (cdr var_list) (cdr val_list)))))))

; Gets the value of a variable in the state
(define get_var
  (lambda (var state)
    (printf "get_var called with var: ~a and state: ~a\n" var state)
    (cond
      ((not (var_exists? var state)) (error (format "Variable not declared: ~a" var)))
      ((var_init? var state) (find_var var state))
      (else (error (format "Variable not initialized: ~a" var))))))

; Helper for get_var, finds var and returns its value
(define find_var
  (lambda (var state)
    (printf "find_var called with var: ~a and state: ~a\n" var state)
    (cond
      ((or (null? (car state)) (null? (cadr state))) (error (format "Variable not in state: ~a" var)))
      ((eq? var (car (car state))) 
        (cond
          ((eq? #t (find_var_helper (car (car (cdr state))))) 'true)
          ((eq? #f (find_var_helper (car (car (cdr state))))) 'false)
          (else (find_var_helper (car (car (cdr state)))))))
      (else (get_var var (cons (cdar state) (list (cdadr state))))))))

; Helper for find_var, checks if var has been intialized
(define find_var_helper
  (lambda (value)
    (printf "find_var_helper called with value: ~a\n" value)
    (cond
      ((eq? value 'null) (error "Variable not initialized"))
      (else value))))
;=======================================
;; Helper Functions
;=======================================
