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
    (M_state (parser input) (init_state) (lambda (x) x) (lambda (x) x))))

;=======================================
;; M_ functions
;; These functions are the main stateful functions that are called by the parser
;=======================================
(define M_state
  (lambda (statement state return break)
    (cond
      ((null? statement) state)
      ((list? (car statement)) (M_state 
                                (cdr statement) 
                                (M_state (car statement) state return break) 
                                return break))
      ((eq? (function statement) 'var) (var_dec 
                                        (varname statement) 
                                        state return break))
      ((eq? (function statement) '=) (var_assn 
                                      (varname statement) 
                                      (varvalue statement) 
                                      state return break))
      ((eq? (function statement) 'while) (M_while 
                                          (condition statement) 
                                          (body1 statement) 
                                          state return break))
      ((eq? (function statement) 'if) (M_if 
                                        (condition statement) 
                                        (body1 statement) 
                                        (body2 statement) 
                                        state return break))
      ((eq? (function statement) 'return) (M_return 
                                            (cadr statement) 
                                            state return break))
      (else (error "Invalid statement")))))

; abstraction
(define function car)

; abstraction for declare/assign
(define varname cadr)
(define varvalue caddr)

; abstraction for if/while
(define condition cadr)
(define body1 caddr)
(define body2 cadddr)


(define M_if
  (lambda (if_statement if_then if_else state return break)
    (if (M_boolean if_statement)
        (M_state (if_then (M_state if_statement state return break)) return break)
        (M_state (if_else (M_state if_statement state return break)) return break))))

(define M_while
  (lambda (while_statement while_body state)
    (if (M_boolean while_statement)
        (M_while while_statement while_body (M_state (while_body (M_state while_statement state))))
        (M_state (while_statement state)))))


; evaluates a mathematical expression
(define M_value
  (lambda (expression state)
    (cond
      ; var
      ((var? expression) (get_var expression state))
      ; mathematical evaluation
      ((number? expression) expression)
      ((eq? '+ (op expression)) (+ (M_value (x expression) (M_value (y expression)))))
      ((eq? '- (op expression)) (- (M_value (x expression) (M_value (y expression)))))
      ((eq? '* (op expression)) (* (M_value (x expression) (M_value (y expression)))))
      ((eq? '/ (op expression)) (quotient (M_value (x expression) (M_value (y expression)))))
      ((eq? '% (op expression)) (remainder (M_value (x expression) (M_value (y expression)))))

      ; logical evaluation
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)

      (else 'error "Invalid expression")

      )))

(define var?
  (lambda (x)
    (and (and (not (number? x)) (not (list? x))) (not (pair? x)))))

; evaluates a boolean expression  ==, !=, <, >, <=. >=
(define M_boolean
  (lambda (expression state)
    (cond
      ((boolean? expression) expression)
      ((eq? '== (op expression)) (eq? (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '!= (op expression)) (not (eq? (M_value (x expression) state) (M_value (y expression) state))))
      ((eq? '> (op expression)) (> (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '< (op expression)) (< (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '>= (op expression)) (>= (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '<= (op expression)) (<= (M_value (x expression) state) (M_value (y expression) state)))
      (else 'error "invalid boolean expression")
      )))



(define op car)
(define x cadr)
(define y caddr)

(define M_return
  (lambda (statement state)
    (M_value statement state)))

;=======================================
;; Variable Logic
;=======================================

; Declares a new variable with no value 
; Returns state with var added to it
(define var_dec
  (lambda (var state)
    (if (var_exists? var state)
        (error "Variable already exists")
        (append_state var 'null state))))

; Declares a new variable with a value 
; TODO: This does not work, must combine with var_dec, to see if it is a var_dec or var_dec_assn
(define var_dec_assn
  (lambda (var val state)
    (if (var_exists? var state)
        (error "Variable already exists")
        (append_state var val state))))

(define var_assn
  (lambda (var val state)
    (if (var_exists? var state)
      (add_binding var val (remove_binding var state))
      (error "Variable does not exist"))))

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

; Checks if a variable exists in the state
; Returns true if it exists, false otherwise
(define var_exists? 
  (lambda (var state)
    (cond 
      ((null? (car state)) #f)
      ((equal? var (car (car state))) #t)
      (else (var_exists? var (cons (cdr (car state)) (cdr state)))))))

; Checks if a variable has been initialized with a value
; Returns true if it has been initialized, false otherwise
(define var_init?
  (lambda (var state)
    (cond
      ((eq? #f (var_exists? var state)) #f)
      ((null? (cdr state)) #f)
      ((equal? var (car (car state))) (not (null? (car (cdr state)))))
      (else (var_init? var (cons (cdr (car state)) (cdr (cdr state))))))))

; Sets binding of var to val in the state
(define remove_binding
  (lambda (var state)
    (list (car state) (find_replace_val var (car state) (car (cdr state))))))

; Helper for remove_binding, finds var and replace its val with null
(define find_replace_val
  (lambda (var var_list val_list)
    (cond
      ((null? var_list) val_list)
      ((equal? var (car var_list)) (cons null (cdr val_list))) ; Set binding to null
      (else (cons (car val_list) (find_replace_val var (cdr var_list) (cdr val_list)))))))

; Sets binding of var to val in the state
(define add_binding
  (lambda (var val state)
    (list (car state) (find_set_val var val (car state) (car (cdr state))))))

; Helper for add_binding, finds var and replace its val with new val
(define find_set_val
  (lambda (var val var_list val_list)
    (cond
      ((null? var_list) val_list) ;
      ((equal? var (car var_list)) (cons val (cdr val_list))) ; Update binding
      (else (cons (car val_list) (find_set_val var val (cdr var_list) (cdr val_list)))))))

; find var in state
(define get_var
  (lambda (var state)
    (if (var_init? var state)
        (find_var var state)
        ('error "var not initialized"))))

(define find_var
  (lambda (var state)
    (cond
      ((or (null? (car state)) (null? (cadr state))) ('error "var not in state"))
      ((eq? var (car (car state))) (car (car (cdr state))))
      (else (get_var var (cons (cdar state) (cadr state)))))))
;=======================================
;; Helper Functions
;=======================================
