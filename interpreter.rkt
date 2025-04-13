#lang racket
(require "simpleParser.rkt")
; (require "functionParser.rkt")
(provide (all-defined-out))


;=====================================================================================================
;; Interpreter 
;; Henry Odza, Tyler Powers, and Benjamin Zheng 
;;
;; CSDS 345
;=====================================================================================================


;=====================================================================================================
;; Verbose Flag and print helper function
;; Used for debugging, set verbose to #t to see print statements
;=====================================================================================================
; Verbose flag to control print statements
(define verbose #f)

; Helper function for conditional printing
(define (vprintf fmt . args)
  (when verbose
    (apply printf fmt args)))


;=====================================================================================================
;; Interpreter
;; The main interpreter function that calls the parser and M_state, formater will take the output of 
;; M_state and format it for the user
;=====================================================================================================
; Calls the parser on the input file 
; Input: input file with code
(define interpret
  (lambda (input)
    (M_state (parser input) init_state d_return d_next d_break d_continue d_throw)))

(define formater
  (lambda (input)
    (vprintf "Formater called with input: ~s\n" input)
    (cond
      ((boolean? input) (if input 'true 'false))
      (else input))))


;=====================================================================================================
;; M_ functions
;; These functions are the main stateful functions that are called by the parser
;=====================================================================================================

(define M_state
  (lambda (statement state return next break continue throw)
    (vprintf "M_state called, statement: ~s, state: ~s, next: ~s, break: ~s, continue: ~s, throw: ~s\n" statement state next break continue throw)
    (cond
      ((null? statement) (next state))
      
      ; ((list? (function statement))
      ;   (M_state (function statement) state
      ;     (lambda (v1)
      ;       (M_state (stmt_list statement) v1 return next break continue throw))
      ;       next break continue throw))
      ((list? (function statement)) (M_state
                                     (function statement) state return (lambda (v)
                                                                         (M_state (stmt_list statement) v return next break continue throw))
                                     break continue throw))
      
      ((eq? (function statement) 'begin)
       (M_state (stmt_list statement) 
                (add_nested_state state) 
                return
                (lambda (v) (next (remove_nested_state v))) 
                (lambda (v) (break (remove_nested_state v))) 
                (lambda (v) (continue (remove_nested_state v)))
                throw))
      
      ((eq? (function statement) 'var) (next (M_declare
                                              statement
                                              state)))
      ((eq? (function statement) '=) (next (M_assign
                                           statement
                                           state)))
      ((eq? (function statement) 'while) (M_while 
                                          (condition statement) 
                                          (body statement) 
                                          state return next break continue throw))
      ((eq? (function statement) 'if) (M_if statement state return next break continue throw))
      ((eq? (function statement) 'return) (return (M_return
                                                   (return_val statement)
                                                   state)))
      ((eq? (function statement) 'break) (break state))
      ((eq? (function statement) 'continue) (continue state))
      ((eq? (function statement) 'throw) (throw state (value statement)))
      ((eq? (function statement) 'try) (M_try (stmt_list statement) 
                                              (add_nested_state state) 
                                              return 
                                              (lambda (v) (next (remove_nested_state v))) 
                                              (lambda (v) (break (remove_nested_state v))) 
                                              (lambda (v) (continue (remove_nested_state v))) 
                                              throw))

      (else (error (format "Invalid statement: ~s" statement))))))

(define M_try
  (lambda (statement state return next break continue throw)
    (vprintf "M_try called with try_statement: ~s, state: ~s\n" statement state)
    (vprintf "HHHHHHEEEEERRERERE cadr of statement: ~s\n" (cadr statement))
    (M_state (tryblock statement) state return         
             (if (not (null? (cadr statement)))
                  (if (not (null? (caddr statement)))
                      (lambda (v) (M_state (finallyblock statement) v return next break continue throw))
                      next)
                  (lambda (v) (M_state (earlyfinallyblock statement) v return next break continue throw)))
              break continue
              (if (not (null? (cadr statement)))
                  (lambda (v1 v2) (M_state (catchblock statement) 
                                           (add_catch_state v1 (car (cadadr statement)) (car v2))
                                           return
                                           (if (not (null? (caddr statement)))
                                               (lambda (v) (M_state (finallyblock statement) v return next break continue throw))
                                               next)
                                           break continue throw))
                  (lambda (v) (M_state (earlyfinallyblock statement) v return next break continue throw)))
              )))

(define add_catch_state
  (lambda (state var val)
    (vprintf "add_catch_state called with state: ~s, var: ~s, val: ~s\n" state var val)
    (append_state var val state)))
  
; if statement. If condition is true,
(define M_if
  (lambda (statement state return next break continue throw)
    (vprintf "M_if called with statement: ~s, state: ~s\n" statement state)
    (if (M_boolean (condition statement) state)
        (M_state (body1 statement) state return next break continue throw)
        (if (> 4 (length statement))
            (next state)
            (M_state (body2 statement) state return next break continue throw)))))
; (define M_if
;   (lambda (statement state return next break continue throw)
;     (vprintf "M_if called with statement: ~s, state: ~s\n" statement state)
;     (if (M_boolean (condition statement) state)
;         (M_state (body1 statement) state return next break continue throw)
;         (M_state (body2 statement) state return next break continue throw))))

; while statement. While condition is true
; (define M_while
;   (lambda (while_statement while_body state return next break continue throw)
;     (vprintf "M_while called with cond: ~s, body: ~s, st: ~s\n"
;              while_statement while_body state)
;     (if (M_boolean while_statement state)
;         (M_while while_statement while_body 
;                  (M_state while_body state return next break continue throw)
;                  return next break continue throw)
;         (next state))))
; (define M_while
;   (lambda (while_statement while_body state return next break continue throw)
;     (vprintf "M_while called with while_statement: ~s, while_body: ~s, state: ~s\n" 
;               while_statement while_body state)
;     (if (M_boolean while_statement state)
;         (M_while while_statement while_body (M_state while_body state return (lambda (v) v) break continue throw) return (lambda (v) v) break continue throw)
;         (next state))))
(define M_while
  (lambda (while_statement while_body state return next break continue throw)
    (vprintf "M_while called with while_statement: ~s, while_body: ~s, state: ~s\n" 
              while_statement while_body state)
    (if (M_boolean while_statement state)
        (M_state while_body state return 
                 (lambda (v) (M_while while_statement while_body v return next break continue throw)) 
                 (lambda (v) (next v))
                 (lambda (v) (M_while while_statement while_body v return next break continue throw))
                 throw)
        (next state))))
; declare a variable
(define M_declare
  (lambda (statement state)
    (vprintf "M_declare called with statement: ~s and state: ~s\n" statement state)
    (cond
      ((var_exists? (varname statement) state) (error ("Variable already exists")))
      ((has_value? statement) (var_dec_assn 
                                (varname statement) 
                                (M_value (varvalue statement) state) state))
      (else (var_dec (varname statement) state)))))

; assigns a value to a variable
(define M_assign
  (lambda (statement state)
    (vprintf "M_assign called with statement: ~s and state: ~s\n" statement state)
    (if (var_exists? (varname statement) state)
      (var_assn (varname statement) (M_value (varvalue statement) state) state)
      (error (format "Variable not declared: ~s" (varname statement))))))



; evaluates a mathematical expression
(define M_value
  (lambda (expression state)
    (vprintf "M_value called with expression: ~s and state: ~s\n" expression state)
    (cond
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((boolean? expression) expression)
      ((number? expression) expression)
      ((var? expression) (get_var expression state))
      ((eq? '|| (op expression)) (or 
                                  (M_boolean (x expression) state) 
                                  (M_boolean (y expression) state)))
      ((eq? '&& (op expression)) (and 
                                  (M_boolean (x expression) state) 
                                  (M_boolean (y expression) state)))
      ((eq? '! (op expression)) (not (M_boolean (x expression) state)))
      ((eq? '== (op expression)) (eq? (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '!= (op expression)) (not (eq? 
                                        (M_value (x expression) state) 
                                        (M_value (y expression) state))))
      ((eq? '> (op expression)) (> (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '< (op expression)) (< (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '>= (op expression)) (>= (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '<= (op expression)) (<= (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '+ (op expression)) (+ (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '- (op expression)) (subtract expression state))
      ((eq? '* (op expression)) (* (M_value (x expression) state) (M_value (y expression) state)))
      ((eq? '/ (op expression)) (quotient 
                                  (M_value (x expression) state) 
                                  (M_value (y expression) state)))
      ((eq? '% (op expression)) (remainder 
                                  (M_value (x expression) state) 
                                  (M_value (y expression) state)))

      (else (error "Invalid expression")))))

; evaluates a boolean expression  ==, !=, <, >, <=. >=
(define M_boolean
  (lambda (expression state)
    (vprintf "M_boolean called with expression: ~s and state: ~s\n" expression state)
    (cond
      ((boolean? expression) expression)
      ((var? expression) (get_var expression state))
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((eq? '== (op expression)) (eq? 
                                  (M_value (x expression) state) 
                                  (M_value (y expression) state)))
      ((eq? '!= (op expression)) (not (eq? 
                                        (M_value (x expression) state) 
                                        (M_value (y expression) state))))
      ((eq? '> (op expression)) (> 
                                  (M_value (x expression) state) 
                                  (M_value (y expression) state)))
      ((eq? '< (op expression)) (< 
                                  (M_value (x expression) state) 
                                  (M_value (y expression) state)))
      ((eq? '>= (op expression)) (>= 
                                  (M_value (x expression) state) 
                                  (M_value (y expression) state)))
      ((eq? '<= (op expression)) (<= 
                                  (M_value (x expression) state) 
                                  (M_value (y expression) state)))
      ((eq? '&& (op expression)) (and 
                                  (M_boolean (x expression) state) 
                                  (M_boolean (y expression) state)))
      ((eq? '|| (op expression)) (or 
                                  (M_boolean (x expression) state) 
                                  (M_boolean (y expression) state)))
      ((eq? '! (op expression)) (not (M_boolean (x expression) state)))
      (else (error "invalid boolean expression"))
      )))

; return statement
(define M_return
  (lambda (statement state)
    (vprintf "M_return called with statement: ~s, state: ~s\n" statement state)
    (formater (M_value statement state))))


;=====================================================================================================
;; Variable Logic
;=====================================================================================================
; Variable declaration, assigns var to null 
(define var_dec
  (lambda (var state)
    (vprintf "var_dec called with var: ~s and state: ~s\n" var state)
    (append_state var 'null state)))

; Variable declaration with assignment, assigns var to val
(define var_dec_assn
  (lambda (var val state)
    (vprintf "var_dec_assn called with var: ~s, val: ~s and state: ~s\n" var val state)
    (append_state var val state)))

(define var_assn
  (lambda (var val state)
    (vprintf "var_assn called with var: ~s, val: ~s and state: ~s\n" var val state)
    (if (var_exists? var state)
        (add_binding var (M_value val state) state)
      (error (format "Variable not declared: ~s" var)))))


;=====================================================================================================
;; State Logic
;=====================================================================================================
; Adds a nested state
(define add_nested_state
  (lambda (state)
    (vprintf "add_nested_state called with state: ~s\n" state)
    (cons (cons '() (vars state)) (list (cons '() (values state))))))

; Removes a nested state
(define remove_nested_state
  (lambda (state)
    (vprintf "remove_nested_state called with state: ~s\n" state)
    (cons (cdar state) (list (cdadr state)))))

; Calls append_var and append_val to map val to var within state
(define append_state
  (lambda (var val old_state)
    (vprintf "append_state called with var: ~s, val: ~s and old_state: ~s\n" var val old_state)
    (cons (append_var var (vars old_state))
          (list (append_val val (values old_state))))))

; Appends a value to the value list within state
(define append_var
  (lambda (var var_list)
    (vprintf "append_var called with var: ~s and var_list: ~s\n" var var_list)
    (cond
      ((null? var_list) (cons var var_list))
      ((list? (car var_list)) (cons (append_var var (car var_list)) (cdr var_list)))
      (else (cons var var_list)))))

; Appends a variable to the variable list within state
(define append_val
  (lambda (val val_list)
    (vprintf "append_val2 called with val: ~s and val_list: ~s\n" val val_list)
    (cond
      ((null? val_list) (cons (box val) val_list))
      ((list? (car val_list)) (cons (append_val val (car val_list)) (cdr val_list)))
      (else (cons (box val) val_list)))))

; Sets binding of var to val in the state
(define add_binding
  (lambda (var val state)
    (vprintf "add_binding called with var: ~s, val: ~s, state: ~s\n" var val state)
    (list (vars state) (find_set_val var val (vars state) (values state)))))

; Helper for add_binding, finds var and replace its val with new val
(define find_set_val
  (lambda (var val var_list val_list)
    (vprintf "find_set_val called with var: ~s, val: ~s, var_list: ~s, val_list: ~s\n" 
              var val var_list val_list)
    (cond
      ((null? var_list) val_list) ;
      ((list? (first_item var_list)) (cons 
                                      (find_set_val var val (first_item var_list) (first_item val_list))
                                      (find_set_val var val (next_item var_list) (next_item val_list))))
      ((equal? var (vars var_list)) (begin (set-box! (car val_list) val) val_list)) ; Update binding
      (else (cons (vars val_list) 
                  (find_set_val var val (next_item var_list) (next_item val_list)))))))

; Gets the value of a variable in the state
(define get_var
  (lambda (var state)
    (vprintf "get_var called with var: ~s and state: ~s\n" var state)
    (cond
      ((not (var_exists? var state)) (error (format "Variable not declared: ~s" var)))
      ((var_init? var state) (find_var var state))
      (else (error (format "Variable not initialized: ~s" var))))))

; Helper for get_var, finds var and returns its value
(define find_var
  (lambda (var state)
    (vprintf "find_var called with var: ~s and state: ~s\n" var state)  
    (cond
      ((or (null? (vars state)) (null? (values state))) (error 
                                                          (format "Variable not in state: ~s" var)))
      ((list? (first_var state)) (if (find_nested_var var (top_state state))
                                          (find_var var (top_state state))
                                          (find_var var (remain_state state))))
      ((eq? var (first_var state)) (find_var_helper (unbox (first_value state))))
      (else (find_var var (remain_state state)))))) 

(define find_nested_var
  (lambda (var state)
    (vprintf "find_nested_var called with var: ~s and state: ~s\n" var state)
    (cond
      ((null? (vars state)) #f)
      ((list? (first_var state)) (or (find_nested_var var (top_state state)) 
                                      (find_nested_var var (remain_state state))))
      ((equal? var (first_var state)) #t)
      (else (find_nested_var var (remain_state state))))))

;=====================================================================================================
;; Helper Functions
;=====================================================================================================

; checks to see if a var declaration has a value to assign
(define has_value? 
  (lambda (statement) 
    (vprintf "has_value? called with statement: ~s\n" statement)
    (if (null? (cddr statement)) #f #t)))

; Checks to see if subtraction is a unary or binary operation
(define subtract
  (lambda (expression state)
    (vprintf "subtract called with expression: ~s and state: ~s\n" expression state)
    (cond 
      ((null? (unary expression)) (- (M_value (x expression) state)))
      (else (- (M_value (x expression) state) (M_value (y expression) state))))))

; Checks to see if a atom could be variable
(define var?
  (lambda (x)
    (vprintf "var? called with x: ~s\n" x)
    (and (and (not (number? x)) (not (list? x))) (and (not (pair? x)) (not (bool_op? x))))))

; Checks to see if a atom is a boolean operator
(define bool_op?
  (lambda (x)
    (vprintf "bool_op? called with x: ~s\n" x)
    (or (eq? 'false x) 
        (or (eq? 'true x) 
            (or (eq? '|| x) 
                (or (eq? '&& x) 
                    (or (eq? '! x) 
                        (or (eq? '== x) 
                            (or (eq? '!= x) 
                                (or (eq? '> x) 
                                    (or (eq? '< x) 
                                        (or (eq? '>= x) 
                                            (or (eq? '<= x) (boolean? x))))))))))))))

; Checks if a variable exists in the state
; Returns true if it exists, false otherwise
(define var_exists? 
  (lambda (var state)
    (vprintf "var_exists? called with var: ~s and state: ~s\n" var state)
    (cond 
      ((null? (vars state)) #f)
      ((list? (first_var state)) (or (var_exists? var (top_state state)) 
                                      (var_exists? var (remain_state state))))
      ((equal? var (first_var state)) #t)
      (else (var_exists? var (remain_state state))))))

; Checks if a variable has been initialized with a value
; Returns true if it has been initialized, false otherwise
(define var_init?
  (lambda (var state)
    (vprintf "var_init? called with var: ~s and state: ~s\n" var state)
    (cond
      ((null? (vars state)) #f)
      ((list? (first_var state)) (or (var_init? var (top_state state)) 
                                      (var_init? var (remain_state state))))
      ((eq? #f (var_exists? var state)) #f)
      ((null? (next_item state)) #f)
      ((equal? var (first_var state)) (not (null? (values state))))
      (else (var_init? var (cons (other_vars state) (list (other_values state))))))))

; Helper for find_var, checks if var has been intialized
(define find_var_helper
  (lambda (value)
    (vprintf "find_var_helper called with value: ~s\n" value)
    (cond
      ((eq? value 'null) (error "Variable not initialized"))
      (else value))))

; Generates state for first block in state
(define top_state
  (lambda (state)
    (vprintf "top_state called with state: ~s\n" state)
    (cons (first_var state) (list (first_value state)))))

; Generates state for excluding first block in state
(define remain_state
  (lambda (state)
    (vprintf "remain_state called with state: ~s\n" state)
    (cons (other_vars state) (list (other_values state)))))


;=====================================================================================================
;; Abstractions
;=====================================================================================================
; abstraction for M_state
(define function car)
(define inner_statement car)
(define stmt_list cdr)
(define return_val cadr)
(define body caddr)
(define condition cadr)
(define value cdr)
(define tryblock car)
(define catchblock cddadr)
(define finallyblock cdaddr)
(define earlyfinallyblock cdaddr)

; abstraction for if/while
(define body1 caddr)
(define body2 cadddr)

; abstraction for declare and assign
(define varname cadr)
(define varvalue caddr)

; abstraction for M_value and M_boolean
(define op car)
(define x cadr)
(define y caddr)
(define unary cddr)

; Abstraction for state logic
(define vars car)
(define values cadr)
(define first_var caar)
(define first_value caadr)
(define other_vars cdar)
(define other_values cdadr)
(define next_item cdr)
(define first_item car)

; The state of the interpreter. Starts empty
; Format (var_list val_list)
(define init_state '(() ()))

; Standard defaults for return, break, continue, and throw
(define d_return (lambda (v) v))
(define d_next (lambda (v) v))
(define d_break (lambda (v) v))
(define d_continue (lambda (v) v))
(define d_throw (lambda (v1 v2) v1 v2))
