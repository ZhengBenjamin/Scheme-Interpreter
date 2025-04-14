#lang racket
; (require "simpleParser.rkt")
(require "functionParser.rkt")
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
    (vprintf "Formater called with input: ~s\n\n" input)
    (cond
      ((boolean? input) (if input 'true 'false))
      (else input))))


;=====================================================================================================
;; M_ functions
;; These functions are the main stateful functions that are called by the parser
;=====================================================================================================

(define M_state
  (lambda (statements state return next break continue throw)
    (vprintf "M_state called, statements: ~s, state: ~s, \n\n" statements state )
    
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
    (vprintf "M_exec called with statement: ~s, state: ~s\n\n" statement state )

    (cond

      ((eq? (function statement) 'begin)
        (M_state (stmt_list statement) 
          (add_nested_state state) 
          (lambda (val) (return val))
          (lambda (new_state) (next (remove_nested_state new_state)))
          (lambda (new_state) (break (remove_nested_state new_state)))
          (lambda (new_state) (continue (remove_nested_state new_state)))
          (lambda (val new_state) (throw val (remove_nested_state new_state)))))
      
      ((eq? (function statement) 'function)
        (cond
          ((eq? (func_name statement) 'main) (M_state (func_body statement) state return next break continue throw)) ; if main func, execute 
          (else (next (M_function_dec (func_name statement) (arg_list statement) (func_body statement) state)))))

      ((eq? (function statement) 'funcall)
        (M_function (func_name statement) (arg_list_closure statement) state (lambda (val) (next state)) return break continue throw))

      ((eq? (function statement) 'var)
        (next (M_declare statement state throw)))

      ((eq? (function statement) '=)
        (next (M_assign statement state throw)))

      ((eq? (function statement) 'while)
        (M_while (condition statement) (ensure_stmt_list (body statement)) state next return break continue throw))

      ((eq? (function statement) 'if)
        (M_if statement state next return break continue throw))

      ((eq? (function statement) 'return)
        (return (M_return (return_val statement) state throw)))

      ((eq? (function statement) 'break)
        (break state))

      ((eq? (function statement) 'continue)
        (continue state))

      ((eq? (function statement) 'throw)
        (throw (M_value (return_val statement) state throw) state))

      ((eq? (function statement) 'try)
        (M_try (try_body statement) (catch_body statement) (finally_clause statement) state next return break continue throw))
      
      (else (error (format "Invalid statement: ~s" statement))))))

(define add_catch_state
  (lambda (state var val)
    (vprintf "add_catch_state called with state: ~s, var: ~s, val: ~s\n\n" state var val)
    (append_state var val state)))

; declare function
(define M_function_dec
  (lambda (name args body state)
    (vprintf "M_function_dec called with name: ~s, args: ~s, body: ~s\n\n" name args body)
      (if (not (check_func_exists? name state))
        (add_closure name args body state)
        (error (format "Function already declared: ~s" name)))))

; function call
(define M_function
  (lambda (name params state next return break continue throw)
    (vprintf "M_function called with name: ~s, params: ~s, state: ~s\n\n" name params state)
    (if (not (check_func_exists? name state))
      (error (format "Function not found: ~s" name))
      (apply-closure (find_closure name (closure_list state))
        params state next return break continue throw))))

; if statement. If condition is true,
(define M_if
  (lambda (statement state next return break continue throw)
    (vprintf "M_if called with statement: ~s, state: ~s\n\n" statement state)
    (if (M_boolean (condition statement) state throw)
        (M_state (ensure_stmt_list (body1 statement)) state return next break continue throw)
        (if (> 4 (length statement))
            (next state )
            (M_state (ensure_stmt_list (body2 statement)) state return next break continue throw)))))

; while statement. While condition is true
(define M_while
  (lambda (cond body state next return break continue throw)
    (vprintf "M_while called with cond: ~s, body: ~s, state: ~s\n\n" cond body state) 
      (if (M_boolean cond state throw)
        (M_state body state return
          (lambda (new_state) (M_while cond body new_state next return break continue throw)) ; next
          (lambda (new_state) (next new_state)) ; break
          (lambda (new_state) (M_while cond body new_state next return break continue throw)) ;continue
          throw)
        (next state))))

; declare a variable
(define M_declare
  (lambda (statement state throw)
    (vprintf "M_declare called with statement: ~s and state: ~s\n\n" statement state)
    (cond
      ((has_value? statement) (var_dec_assn (varname statement) (M_value (varvalue statement) state throw) state))
      (else (var_dec (varname statement) state)))))

; assigns a value to a variable
(define M_assign
  (lambda (statement state throw)
    (vprintf "M_assign called with statement: ~s and state: ~s\n\n" statement state)
    (if (var_exists? (varname statement) state)
        (var_assn (varname statement) (M_value (varvalue statement) state throw) state throw)
        (error (format "Variable not declared or out of scope: ~s" (varname statement))))))

; evaluates a mathematical expression
(define M_value
  (lambda (expression state throw)
    (vprintf "M_value called with expression: ~s and state: ~s\n\n" expression state)
    (cond
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((boolean? expression) expression)
      ((number? expression) expression)
      ((var? expression) (get_var expression state))
      ((eq? '|| (op expression)) (or 
                                  (M_boolean (x expression) state throw) 
                                  (M_boolean (y expression) state throw)))
      ((eq? '&& (op expression)) (and 
                                  (M_boolean (x expression) state throw) 
                                  (M_boolean (y expression) state throw)))
      ((eq? '! (op expression)) (not (M_boolean (x expression) state throw)))
      ((eq? '== (op expression)) (eq? (M_value (x expression) state throw) (M_value (y expression) state throw)))
      ((eq? '!= (op expression)) (not (eq? 
                                        (M_value (x expression) state throw) 
                                        (M_value (y expression) state throw))))
      ((eq? '> (op expression)) (> (M_value (x expression) state throw) (M_value (y expression) state throw)))
      ((eq? '< (op expression)) (< (M_value (x expression) state throw) (M_value (y expression) state throw)))
      ((eq? '>= (op expression)) (>= (M_value (x expression) state throw) (M_value (y expression) state throw)))
      ((eq? '<= (op expression)) (<= (M_value (x expression) state throw) (M_value (y expression) state throw)))
      ((eq? '+ (op expression)) (+ (M_value (x expression) state throw) (M_value (y expression) state throw)))
      ((eq? '- (op expression)) (subtract expression state throw))
      ((eq? '* (op expression)) (* (M_value (x expression) state throw) (M_value (y expression) state throw)))
      ((eq? '/ (op expression)) (quotient 
                                  (M_value (x expression) state throw) 
                                  (M_value (y expression) state throw)))
      ((eq? '% (op expression)) (remainder 
                                  (M_value (x expression) state throw) 
                                  (M_value (y expression) state throw)))

      ((eq? 'funcall (function expression)) 
      (M_function (func_name expression) 
          (arg_list_closure expression) state d_return d_next d_break d_continue d_throw))

      (else (error "Invalid expression")))))

; evaluates a boolean expression  ==, !=, <, >, <=. >=
(define M_boolean
  (lambda (expression state throw)
    (vprintf "M_boolean called with expression: ~s and state: ~s\n\n" expression state)
    (cond
      ((boolean? expression) expression)
      ((var? expression) (get_var expression state))
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((eq? '== (op expression)) (eq? 
                                  (M_value (x expression) state throw) 
                                  (M_value (y expression) state throw)))
      ((eq? '!= (op expression)) (not (eq? 
                                        (M_value (x expression) state throw) 
                                        (M_value (y expression) state throw))))
      ((eq? '> (op expression)) (> 
                                  (M_value (x expression) state throw) 
                                  (M_value (y expression) state throw)))
      ((eq? '< (op expression)) (< 
                                  (M_value (x expression) state throw) 
                                  (M_value (y expression) state throw)))
      ((eq? '>= (op expression)) (>= 
                                  (M_value (x expression) state throw) 
                                  (M_value (y expression) state throw)))
      ((eq? '<= (op expression)) (<= 
                                  (M_value (x expression) state throw) 
                                  (M_value (y expression) state throw)))
      ((eq? '&& (op expression)) (and 
                                  (M_boolean (x expression) state throw) 
                                  (M_boolean (y expression) state throw)))
      ((eq? '|| (op expression)) (or 
                                  (M_boolean (x expression) state throw) 
                                  (M_boolean (y expression) state throw)))
      ((eq? '! (op expression)) (not (M_boolean (x expression) state throw)))

      ((eq? 'funcall (function expression))
        (M_function (func_name expression) 
            (arg_list_closure expression) state d_return d_next d_break d_continue d_throw))

      (else (error "invalid boolean expression"))
      )))

; return statement
(define M_return
  (lambda (statement state throw)
    (vprintf "M_return called with statement: ~s, state: ~s\n\n" statement state)
    (cond
      ((and (list? statement) (eq? (car statement) 'funcall)) 
        (M_function (func_name statement) 
                    (arg_list_closure statement) state
                    (lambda (v) (formater v))
                    (lambda (v) v)
                    d_break d_continue throw))
      (else (formater (M_value statement state throw))))))


; try catch statement
(define M_try
  (lambda (main-stmt catch-stmt finally-stmt state next return break continue throw)
    (M_state main-stmt state return
      (lambda (after-main-state) ;next > eval main statement
        (M_try-normal after-main-state finally-stmt return next break continue throw))
      break continue
      (lambda (val throw-state) ; throw > checks catch > finally
        (if (not (hasCatch? catch-stmt))
            ; No catch > eval finally
            (M_try-no-catch val throw-state finally-stmt return break continue throw next)
            ; Catch > eval catch > finally
            (M_try-catch val throw-state catch-stmt finally-stmt return break continue throw next))))))

(define M_try-normal
  (lambda (after-main-state finally-stmt return next break continue throw)
    (M_state finally-stmt after-main-state return
      (lambda (final_state) (next final_state ))
      break continue throw)))

(define M_try-no-catch
  (lambda (val throw-state finally-stmt return break continue throw next)
    (M_state finally-stmt throw-state return
      (lambda (state2) (throw val state2))
      break continue
      (lambda (val2 state3) (throw val2 state3)))))

(define M_try-catch
  (lambda (val throw-state catch-stmt finally-stmt return break continue throw next)
    (M_state (catch_body catch-stmt)
      (add_nested_state (append_state (catch_var_name catch-stmt) val throw-state))
      return
      (lambda (after-catch-state)
        (M_state finally-stmt after-catch-state return
          (lambda (final_state) (next (remove_nested_state final_state)))
          break continue
          (lambda (val2 state-rethrow)
            (throw val2 (remove_nested_state state-rethrow)))))
      break continue
      (lambda (val2 state-rethrow2)
        (throw val2 (remove_nested_state state-rethrow2))))))


;=====================================================================================================
;; Variable Logic
;=====================================================================================================
; Variable declaration, assigns var to null 
(define var_dec
  (lambda (var state)
    ; (vprintf "var_dec called with var: ~s and state: ~s\n\n" var state)
    (append_state var 'null state)))

; Variable declaration with assignment, assigns var to val
(define var_dec_assn
  (lambda (var val state)
    ; (vprintf "var_dec_assn called with var: ~s, val: ~s and state: ~s\n\n" var val state)
    (append_state var val state)))

(define var_assn
  (lambda (var val state throw)
    ; (vprintf "var_assn called with var: ~s, val: ~s and state: ~s\n\n" var val state)
    (if (var_exists? var state)
        (add_binding var (M_value val state throw) (remove_binding var state))
      (error (format "Variable not declared: ~s" var)))))


;=====================================================================================================
;; State Logic
;=====================================================================================================
; Adds a nested state
(define add_nested_state
  (lambda (state)
    (list
      (cons '() (vars state)) 
      (cons '() (values state)) 
      (closure_list state)))) 


; Removes a nested state
(define remove_nested_state
  (lambda (state)
    (list
      (cdr (vars state))
      (cdr (values state))
      (closure_list state)))) 


; Calls append_var and append_val to map val to var within state
(define append_state
  (lambda (var val old_state)
    (list
      (append_var var (vars old_state))
      (append_val val (values old_state))
      (closure_list old_state))))

; Appends a value to the value list within state
(define append_var
  (lambda (var var_list)
    ; (vprintf "append_var called with var: ~s and var_list: ~s\n\n" var var_list)
    (cond
      ((null? var_list) (cons var var_list))
      ((list? (car var_list)) (cons (append_var var (car var_list)) (cdr var_list)))
      (else (cons var var_list)))))

; Appends a variable to the variable list within state
(define append_val
  (lambda (val val_list)
    ; (vprintf "append_val called with val: ~s and val_list: ~s\n\n" val val_list)
    (cond
      ((null? val_list) (cons val val_list))
      ((list? (car val_list)) (cons (append_val val (car val_list)) (cdr val_list)))
      (else (cons val val_list)))))

; Sets binding of var to val in the state
(define remove_binding
  (lambda (var state)
    (list
      (vars state)
      (find_replace_val var (vars state) (values state))
      (closure_list state))))

; Helper for remove_binding, finds var and replace its val with null
(define find_replace_val
  (lambda (var var_list val_list)
    ; (vprintf "find_replace_val called with var: ~s, var_list: ~s, val_list: ~s\n\n" 
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
    ; Ensure the variable is updated in the correct scope
    (list
      (vars state)
      (find_set_val var val (vars state) (values state))
      (closure_list state))))

; Helper for add_binding, finds var and replace its val with new val
(define find_set_val
  (lambda (var val var_list val_list)
    ; Update the variable in the correct nested scope
    (cond
      ((null? var_list) val_list)
      ((list? (first_item var_list)) 
       (cons 
         (find_set_val var val (first_item var_list) (first_item val_list))
         (find_set_val var val (next_item var_list) (next_item val_list))))
      ((equal? var (first_item var_list)) 
       (cons val (next_item val_list))) ; Update binding
      (else 
       (cons (first_item val_list) 
             (find_set_val var val (next_item var_list) (next_item val_list)))))))

; Gets the value of a variable in the state
(define get_var
  (lambda (var state)
    ; (vprintf "get_var called with var: ~s and state: ~s\n\n" var state)
    (cond
      ((not (var_exists? var state)) (error (format "Variable not declared: ~s" var)))
      ((var_init? var state) (find_var var state))
      (else (error (format "Variable not initialized: ~s" var))))))

; Helper for get_var, finds var and returns its value
(define find_var
  (lambda (var state)
    ; (vprintf "find_var called with var: ~s and state: ~s\n\n" var state)  
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
    ; (vprintf "find_nested_var called with var: ~s and state: ~s\n\n" var state)
    (cond
      ((null? (vars state)) #f)
      ((list? (first_var state)) (or (find_nested_var var (top_state state)) 
                                      (find_nested_var var (remain_state state))))
      ((equal? var (first_var state)) #t)
      (else (find_nested_var var (remain_state state))))))

;=====================================================================================================
;; Function Closure and Environment 
;=====================================================================================================

; adds closure tuple to state
(define add_closure
  (lambda (name args body state)
    (final_state ; replace temp closure with final closure (3) 
      (temp_state state name args body) ; makes temp state with placeholder in closure list (2)
      (make_final_closure name args body ; create new environment to replace placeholder (1)
        (temp_state state name args body))))) 

; check if function exists in state
(define check_func_exists?
  (lambda (name state)
    ; (vprintf "check_func_exists? called with name: ~s and state: ~s\n\n" name state) 
    (cond 
      ((null? (closure_list state)) #f)
      (else (search_closure_list name (closure_list state))))))

; helper for check_func_exists
(define search_closure_list
  (lambda (name closure_list)
    (vprintf "search_closure_list called with name: ~s and closure_list: ~s\n\n" name closure_list)
    (cond 
      ((null? closure_list) #f)
      ((equal? name (closure_name (first_item closure_list))) #t)
      (else (search_closure_list name (next_item closure_list))))))

; find closure 
(define find_closure
  (lambda (name closure_list)
    (cond 
      ((null? closure_list) (error (format "Function not found: ~s" name)))
      ((equal? name (closure_name (first_item closure_list))) (first_item closure_list))
      (else (find_closure name (next_item closure_list))))))

; binding arguments to formal params 
(define bind_parameters
  (lambda (formals actual func_state state throw)
    (vprintf "bind_parameters called with formals: ~s, actual: ~s, fstate: ~s, state: ~s\n\n" formals actual func_state state)
    (cond 
      ((null? formals) func_state)
      ((not (check_input_params formals actual)) (error (format "Function call parameter mismatch: ~s" actual)))
      (else (bind_parameters 
              (cdr formals)
              (cdr actual)
              (append_state (car formals) (M_value (car actual) state throw) func_state)
              state throw)))))

(define check_input_params
  (lambda (formals actual)
    (vprintf "check_input_params called with formals: ~s, actual: ~s\n\n" formals actual)
    (cond
      ((and (null? formals) (null? actual) #t))
      ((not (= (length formals) (length actual))) #f) ; check length different?
      ((not (list? actual)) #f) ; check if expression
      (else (check_input_params (cdr formals) (cdr actual))))))

;=====================================================================================================
;; Helper Functions
;=====================================================================================================

; checks to see if a var declaration has a value to assign
(define has_value? 
  (lambda (statement) 
    ; (vprintf "has_value? called with statement: ~s\n\n" statement)
    (if (null? (cddr statement)) #f #t)))

; Checks to see if subtraction is a unary or binary operation
(define subtract
  (lambda (expression state throw)
    ; (vprintf "subtract called with expression: ~s and state: ~s\n\n" expression state)
    (cond 
      ((null? (unary expression)) (- (M_value (x expression) state throw)))
      (else (- (M_value (x expression) state throw) (M_value (y expression) state throw))))))

; Checks to see if a atom could be variable
(define var?
  (lambda (x)
    ; (vprintf "var? called with x: ~s\n\n" x)
    (and (and (not (number? x)) (not (list? x))) (and (not (pair? x)) (not (bool_op? x))))))

; Checks to see if a atom is a boolean operator
(define bool_op?
  (lambda (x)
    ; (vprintf "bool_op? called with x: ~s\n\n" x)
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
    ; (vprintf "var_exists? called with var: ~s and state: ~s\n\n" var state)
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
    ; (vprintf "var_init? called with var: ~s and state: ~s\n\n" var state)
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
    ; (vprintf "find_var_helper called with value: ~s\n\n" value)
    (cond
      ((eq? value 'null) (error "Variable not initialized"))
      (else value))))

; Generates state for first block in state
(define top_state
  (lambda (state)
    ; (vprintf "top_state called with state: ~s\n\n" state)
    (cons (first_var state) (list (first_value state)))))

; Generates state for excluding first block in state
(define remain_state
  (lambda (state)
    ; (vprintf remain_state called with state: ~s\n\n" state)
    (cons (other_vars state) (list (other_values state)))))

; Takes body from while or if statement and ensures that it is list of statemnt. Wraps into a single element list if not 
(define ensure_stmt_list
  (lambda (statement)
    (if (and (list? statement) (not (null? statement)) (list? (car statement)))
      statement
      (list statement))))

; Takes params from closure and ensures its in list for functions that have one param
(define ensure_param_list
  (lambda (params)
    (if (list? params) params (list params))))

; Checks if try has catch statement
(define hasCatch?
  (lambda (catch_statement)
    (and (pair? catch_statement) (eq? (car catch_statement) 'catch))))

; Rebuild state with new closure list
(define update_state
  (lambda (state new-closure-list)
    (list (vars state) (values state) new-closure-list)))

; Make environment for closure
(define make_closure_env
  (lambda (st)
    (list (vars st) (values st) (closure_list st))))

; Uses temp state to update closure env
(define make_final_closure
  (lambda (name args body st)
    (list name args body (make_closure_env st))))

; Replace temp closure with final closure in state
(define final_state
  (lambda (st finalClosure)
    (list (vars st) (values st) 
          (cons finalClosure (cdr (closure_list st))))))

; Create temp state, cons closure placeholder onto current closure list
(define temp_state
  (lambda (state name args body)
    (update_state state (cons (list name args body #f) (closure_list state)))))

; Makes merged nested environment for closure
(define apply-closure
  (lambda (closure params state next return break continue throw)
    ; Ensure the closure environment is correctly merged with the current state
    (M_state (closure_body closure)
      (bind_parameters (closure_params closure)
        (ensure_param_list params)
        (add_nested_state (list (vars (closure_env closure)) 
                                (values (closure_env closure)) 
                                (closure_list state))) 
        state throw)
      (lambda (val) (next val)) 
      return break continue throw)))

;=====================================================================================================
;; Abstractions
;=====================================================================================================

; abstraction for state logic
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

; abstraction for function parts given closure tuple
(define closure_name car)
(define closure_params cadr)
(define closure_body caddr)
(define closure_env cadddr)

; abstraction from function statement 
(define func_name cadr)
(define func_body cadddr)
(define arg_list caddr)
(define arg_list_closure cddr) ; calling on closure list

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
(define closure_list caddr)

; Abstraction for try catch
(define try_body cadr)
(define catch_clause caddr)
(define finally_clause (lambda (statement) (if (null? (cadddr statement)) '() (cadr (cadddr statement))))) ; gives finally clause, empty if none
(define catch_var_name (lambda (catch_clause) (caadr catch_clause)))
(define catch_body (lambda (catch_clause) (caddr catch_clause))) 

; The state of the interpreter. Starts empty
; Format (var_list val_list closure_tuples)
(define init_state '(() () ()))

; Standard defaults for return, break, continue, and throw
(define d_return (lambda (v) v))
(define d_next (lambda (new_state) new_state))
(define d_break (lambda (v) (error "Invalid break statement")))
(define d_continue (lambda (v) "Invalid continue statement"))
(define d_throw (lambda (val state) (error (format "Uncaught exception: ~s" val))))