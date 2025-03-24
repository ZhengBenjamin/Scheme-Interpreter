#lang racket
(require "simpleParser.rkt")
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
(define verbose #t)

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
  (lambda (statements state return next break continue throw)
    (vprintf "M_state called, statements: ~s, state: ~s\n" statements state )
    
    (cond
      ; Base case no statements, next continuation
      ((null? statements) (next state))

      ; Else execute first statemnet, then call M_state on the rest
      (else (M_exec (function statements) state
              (lambda (new_state)
                (M_state (stmt_list statements) new_state return next break continue throw))
            return break continue throw)))))

(define M_exec
  (lambda (statement state next return break continue throw)
    (vprintf "M_exec called with statement: ~s, state: ~s\n" statement state )

    (cond

      ((eq? (function statement) 'begin)
        (M_state (stmt_list statement) 
          (add_nested_state state) 
          (lambda (val) (return val))
          (lambda (new_state) (next (remove_nested_state new_state)))
          (lambda (new_state) (break (remove_nested_state new_state)))
          (lambda (new_state) (continue (remove_nested_state new_state)))
          (lambda (val new_state) (throw val (remove_nested_state new_state)))))

      ((eq? (function statement) 'var)
        (next (M_declare statement state)))

      ((eq? (function statement) '=)
        (next (M_assign statement state)))

      ((eq? (function statement) 'while)
        (M_while (condition statement) (ensure_stmt_list (body statement)) state next return break continue throw))

      ((eq? (function statement) 'if)
        (M_if statement state next return break continue throw))

      ((eq? (function statement) 'return)
        (return (M_return (return_val statement) state)))

      ((eq? (function statement) 'break)
        (break state))

      ((eq? (function statement) 'continue)
        (continue state))

      ((eq? (function statement) 'throw)
        (throw (M_value (return_val statement) state) state))
      
      (else (error (format "Invalid statement: ~s" statement))))))

; if statement. If condition is true,
(define M_if
  (lambda (statement state next return break continue throw)
    (vprintf "M_if called with statement: ~s, state: ~s\n" statement state)
    (if (M_boolean (condition statement) state)
        (M_state (ensure_stmt_list (body1 statement)) state return next break continue throw)
        (if (> 4 (length statement))
            (next state)
            (M_state (ensure_stmt_list (body2 statement)) state return next break continue throw)))))

; while statement. While condition is true
(define M_while
  (lambda (cond body state next return break continue throw)
    (vprintf "M_while called with cond: ~s, body: ~s, state: ~s\n" cond body state) 
      (if (M_boolean cond state)
        (M_state body state return
          (lambda (new_state) (M_while cond body new_state next return break continue throw)) ; next
          (lambda (new_state) (next new_state)) ; break
          (lambda (new_state) (M_while cond body new_state next return break continue throw)) ;continue
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
        (add_binding var (M_value val state) (remove_binding var state))
      (error (format "Variable not declared: ~s" var)))))


;=====================================================================================================
;; State Logic
;=====================================================================================================
; Adds a nested state
(define add_nested_state
  (lambda (state)
    ; (vprintf "add_nested_state called with state: ~s\n" state)
    (cons (cons '() (vars state)) (list (cons '() (values state))))))

; Removes a nested state
(define remove_nested_state
  (lambda (state)
    ; (vprintf "remove_nested_state called with state: ~s\n" state)
    (cons (cdar state) (list (cdadr state)))))

; Calls append_var and append_val to map val to var within state
(define append_state
  (lambda (var val old_state)
    ; (vprintf "append_state called with var: ~s, val: ~s and old_state: ~s\n" var val old_state)
    (cons (append_var var (vars old_state))
          (list (append_val val (values old_state))))))

; Appends a value to the value list within state
(define append_var
  (lambda (var var_list)
    ; (vprintf "append_var called with var: ~s and var_list: ~s\n" var var_list)
    (cond
      ((null? var_list) (cons var var_list))
      ((list? (car var_list)) (cons (append_var var (car var_list)) (cdr var_list)))
      (else (cons var var_list)))))

; Appends a variable to the variable list within state
(define append_val
  (lambda (val val_list)
    ; (vprintf "append_val called with val: ~s and val_list: ~s\n" val val_list)
    (cond
      ((null? val_list) (cons val val_list))
      ((list? (car val_list)) (cons (append_val val (car val_list)) (cdr val_list)))
      (else (cons val val_list)))))

; Sets binding of var to val in the state
(define remove_binding
  (lambda (var state)
    ; (vprintf "remove_binding called with var: ~s and state: ~s\n" var state)
    (list (vars state) (find_replace_val var (vars state) (values state)))))

; Helper for remove_binding, finds var and replace its val with null
(define find_replace_val
  (lambda (var var_list val_list)
    ; (vprintf "find_replace_val called with var: ~s, var_list: ~s, val_list: ~s\n" 
              ; var var_list val_list)
    (cond
      ((null? var_list) val_list)
      ((list? (first_item var_list)) (cons 
                                      (find_replace_val var (first_item var_list) (first_item val_list))
                                      (find_replace_val var (next_item var_list) (next_item val_list))))
      ((equal? var (vars var_list)) (cons null (cdr val_list))) ; Set binding to null
      (else (cons (vars val_list) 
                  (find_replace_val var (next_item var_list) (next_item val_list)))))))

; Sets binding of var to val in the state
(define add_binding
  (lambda (var val state)
    ; (vprintf "add_binding called with var: ~s, val: ~s, state: ~s\n" var val state)
    (list (vars state) (find_set_val var val (vars state) (values state)))))

; Helper for add_binding, finds var and replace its val with new val
(define find_set_val
  (lambda (var val var_list val_list)
    ; (vprintf "find_set_val called with var: ~s, val: ~s, var_list: ~s, val_list: ~s\n" 
              ; var val var_list val_list)
    (cond
      ((null? var_list) val_list) ;
      ((list? (first_item var_list)) (cons 
                                      (find_set_val var val (first_item var_list) (first_item val_list))
                                      (find_set_val var val (next_item var_list) (next_item val_list))))
      ((equal? var (vars var_list)) (cons val (next_item val_list))) ; Update binding
      (else (cons (vars val_list) 
                  (find_set_val var val (next_item var_list) (next_item val_list)))))))

; Gets the value of a variable in the state
(define get_var
  (lambda (var state)
    ; (vprintf "get_var called with var: ~s and state: ~s\n" var state)
    (cond
      ((not (var_exists? var state)) (error (format "Variable not declared: ~s" var)))
      ((var_init? var state) (find_var var state))
      (else (error (format "Variable not initialized: ~s" var))))))

; Helper for get_var, finds var and returns its value
(define find_var
  (lambda (var state)
    ; (vprintf "find_var called with var: ~s and state: ~s\n" var state)  
    (cond
      ((or (null? (vars state)) (null? (values state))) (error 
                                                          (format "Variable not in state: ~s" var)))
      ((list? (first_var state)) (if (find_nested_var var (top_state state))
                                          (find_var var (top_state state))
                                          (find_var var (remain_state state))))
      ((eq? var (first_var state)) (find_var_helper (first_value state)))
      (else (get_var var (remain_state state))))))

(define find_nested_var
  (lambda (var state)
    ; (vprintf "find_nested_var called with var: ~s and state: ~s\n" var state)
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
(define has_value? (lambda (statement) (if (null? (cddr statement)) #f #t)))

; Checks to see if subtraction is a unary or binary operation
(define subtract
  (lambda (expression state)
    ; (vprintf "subtract called with expression: ~s and state: ~s\n" expression state)
    (cond 
      ((null? (unary expression)) (- (M_value (x expression) state)))
      (else (- (M_value (x expression) state) (M_value (y expression) state))))))

; Checks to see if a atom could be variable
(define var?
  (lambda (x)
    ; (vprintf "var? called with x: ~s\n" x)
    (and (and (not (number? x)) (not (list? x))) (and (not (pair? x)) (not (bool_op? x))))))

; Checks to see if a atom is a boolean operator
(define bool_op?
  (lambda (x)
    ; (vprintf "bool_op? called with x: ~s\n" x)
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
    ; (vprintf "var_exists? called with var: ~s and state: ~s\n" var state)
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
    ; (vprintf "var_init? called with var: ~s and state: ~s\n" var state)
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
    ; (vprintf "find_var_helper called with value: ~s\n" value)
    (cond
      ((eq? value 'null) (error "Variable not initialized"))
      (else value))))

; Generates state for first block in state
(define top_state
  (lambda (state)
    ; (vprintf "top_state called with state: ~s\n" state)
    (cons (first_var state) (list (first_value state)))))

; Generates state for excluding first block in state
(define remain_state
  (lambda (state)
    ; (vprintf remain_state called with state: ~s\n" state)
    (cons (other_vars state) (list (other_values state)))))

; Takes body from while or if statement and ensures that it is list of statemnt. Wraps into a single element list if not 
(define ensure_stmt_list
  (lambda (statement)
    (if (and (list? statement) (not (null? statement)) (list? (car statement)))
      statement
      (list statement))))

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
(define d_break (lambda (v) (error "Invalid break statement")))
(define d_continue (lambda (v) "Invalid continue statement"))
(define d_throw (lambda (val state) (error (format "Uncaught exception: ~s" val))))
