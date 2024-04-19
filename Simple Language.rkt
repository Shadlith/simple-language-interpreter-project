#lang racket

; Group 8 - Suvaion Das, Remy Niman, Faraz Mamaghani
; Simple Language Interpreter

;(require "sectionParser.rkt")
;(require "functionParser.rkt")
(require "classParser.rkt")

(define parse
  (lambda (filename)
    (parser filename)
    ))


#|
((class A () ((var x 5) (var y 10) (static-function main () ((var a (new A)) (return (+ (dot a x) (dot a y))))))))

Test 7: 
((class A () ((var x 1) (var y 2) (function m () ((return (funcall (dot this m2))))) (function m2 () ((return (+ (dot this x) (dot this y)))))))
  (class B
    (extends A)
    ((var y 22) (var z 3) (function m () ((return (funcall (dot super m))))) (function m2 () ((return (+ (+ (dot this x) (dot this y)) (dot this z)))))))
  (class C (extends B) ((var y 222) (var w 4) (function m () ((return (funcall (dot super m))))) (static-function main () ((return (funcall (dot (new C) m))))))))

Test 8:
'((class A () ((function add (g h) ((return (+ g h)))) (static-function main () ((var a (new A)) (return (funcall (dot a add) 10 2)))))))


|#

(define interpret
  (lambda (filename classname)
    (define parsed (parse filename))
    (print parsed);just to see what it outputs
    (newline)
    (let ((x (M_state_function_closure_maker parsed (list (list(list(box 'return))) (list(list(box 'null))) '(()) '(())))))
    (display "closure: ") (print x)
    (answer_converter (M_state_lookup 'return (call/cc (lambda (return1) (M_value_call_function 'main '(())
                           (list (get_element 0 x) (get_element 1 x) (get_element 2 x) (get_element 3 x)) return1 '() '()))) '() '()))) 
    ))

;For Part 3, we had the state have 4 elements:
; Element 0 = declared_list
; Element 1 = value_list
; Element 2 = func_name_list
; Element 3 = func_closure_list

; For Part 3, the function closure contains:
;     formal parameter list
;     the function body
;     a function that creates the function environment from the current environment.

; For Part 4, we're going to have 2 "state" variables variables:
; "class_info" and "state"
; "class_info" contains the following (and is made once and never changed): 
;     Element 0 = class_name_list
;     Element 1 = class_closure_list
; "state" containst he following: 
; You add a layer when you hit a curly bracket.
; Layer 0: Element 0 = declared_list
;          Element 1 = value_list (which will either be a value or an instance closure)
; Layer 1: (same format as Layer 0)

; For Part 4, the method closure contains:
;     formal parameter list (Add "this" as an additional parameter to the parameter list in each (non-static) function's closure.)
;     the body of the method
;     a function that creates the method environment from the current environment.
;     a function (or equivalent information) so that you can look up the function's class in the environment/state.
;           this is the compile time type of "this" in the method body. 


; For Part 4, the class closure contains:
;     super class
;     list of instance field names
;     list of instance values* 
;     list of methods/function names
;     list of methods/function closures
;     (maybe) any nested types (things that extend this function)
; * we added this against instructions


; For Part 4, the instance closure contains:
;     run-time type of the instance
;     instance field names*
;     the values of all instance fields
; * we added this against instructions

; this makes a singular class closure with 5 elements in it
(define M_state_class_closure_maker
  (lambda (lis output)
    (cond
      ((null? lis) output)
      ; if this class does extend another class
      ((and (eq? 'class (car lis)) (not (null? (get_element 2 lis)))) 
       (M_state_class_closure_maker (get_element 3 lis) (list
                                                (get_element 1 (get_element 2 lis))
                                                (get_element 1 output)
                                                (get_element 2 output)
                                                (get_element 3 output)
                                                (get_element 4 output))))
      ; if this class does NOT extend another class
      ((eq? 'class (car lis)) 
       (M_state_class_closure_maker (get_element 3 lis) output))

      ; if we're on a line that declares an instance field
      ((eq? 'var (caar lis))
       (M_state_class_closure_maker (cdr lis) (list
                                               (get_element 0 output)
                                               (cons (get_element 1 (car lis)) (get_element 1 output))
                                               (cons (get_element 2 (car lis)) (get_element 2 output))
                                               (get_element 3 output)
                                               (get_element 4 output))))

      ; if we're on a line for a function or the main, then add the name to the function list and function closure
      ((or (eq? 'function (caar lis)) (eq? 'static-function (caar lis)))
       (M_state_class_closure_maker (cdr lis) (list
                                               (get_element 0 lis)
                                               (get_element 1 output)
                                               (get_element 2 output)
                                               (cons (get_element 1 lis) (get_element 3 output))
                                               (cons (list (get_element 2 lis) (get_element 3 lis) 1) (get_element 4 output)) ;<- 1 is hard-coded b/c we think that we'll always have to pull from layer 1 since we don't have to deal with nested functions
                                               )))
      )))



(define M_state_get_class_closure_info
  (lambda (class_name class_info)
    (cond
      ((null? (get_element 0 class_info)) (error "class doesn't exist"))
      ((eq? class_name (car (get_element 0 class_info))) (car (get_element 1 class_info)))
      (else (M_state_get_class_closure_info class_name (list (cdr (get_element 0 class_info)) (cdr (get_element 1 class_info)))))
      )))


; "lis" is just the list that contains the "new". We want to output a state with an updated value_list where the "value" for the variable
; mapped to this Object is the instance closure. 
(define M_state_instance_closure_maker
  (lambda (class_name class_info) 
   (list class_name #| STUFF|# (M_state_get_class_closure_info class_name class_info))
      ))


;We need to see if we need this....we should just be able to pull from the value list....but we need to figure out when we need that info.
#|
(define M_state_get_instance_closure_info
  (lambda (class_name class_info)
    (cond
      ((null? (get_element 0 class_info)) (error "class doesn't exist"))
      ((eq? class_name (car (get_element 0 class_info))) (car (get_element 1 class_info)))
      (else (M_state_get_class_closure_info class_name (list (cdr (get_element 0 class_info)) (cdr (get_element 1 class_info)))))
      )))
|#


(define M_state_function_closure_maker
  (lambda (lis state)
    (cond
      ((null? lis) state)
      
      ((eq? 'function (caar lis))
       (M_state_function_closure_maker (cdr lis)
                              (list (get_element 0 state) (get_element 1 state)
                              (M_state_add_to_func_name_list (get_element 2 state) (cadar lis))
                              (M_state_add_to_func_closure_list (get_element 3 state)
                                                                (list(get_element 2 (car lis)) (get_element 3 (car lis)) (length (get_element 0 state)))))))

      ; global parameter reading, "evaluate" comes back up here if it hits the word "function"                         
     ((or (eq? 'var (caar lis)) (eq? '= (caar lis))) (evaluate lis state '() '()' ()))
      )))

(define answer_converter
  (lambda (var)
    (cond
      ((and (boolean? var) var) 'true)
      ((boolean? var) 'false)
      (else var))))


(define M_value_call_function
 (lambda (func_name actual_params state return break try)
   (cond 
     ((null? (get_element 2 state)) (error "function not in func_name list"))    
     ((member? func_name (get_element 2 state))
      (call/cc (lambda (return1) (evaluate (get_element 1 (M_state_func_lookup func_name state))
                                           (let ((x (M_state_func_environment_shell (get_element 2 (M_state_func_lookup func_name state))
                                                                                    (get_element 0 (M_state_func_lookup func_name state))
                                                                                    (M_state_actual_param_evaluator actual_params state '() break try) state)))
                                                 (list (car x)
                                                       (cadr x)
                                                       (M_state_add_row_to_list (get_element 2 state))
                                                       (M_state_add_row_to_list (get_element 3 state))))
                                           return1 break try))))
     
     (else (call/cc (lambda (return1) (M_value_call_function func_name actual_params
                                  (list (get_element 0 state) (get_element 1 state) (cdr (get_element 2 state)) (cdr (get_element 3 state))) return1 break try))))
    )))

(define M_state_func_environment_shell
  (lambda (num formal_params actual_params state)
    (cond
      ((not (eq? (length actual_params) (length formal_params)))(error "formal params and actual params are not the same length"))
      (else (M_state_create_func_layer formal_params actual_params
                                       (let ((x (M_state_func_environment state num)))
                                (list (M_state_add_row_to_declared_list (car x))
                                (M_state_add_row_to_value_list (cadr x))
                                (M_state_add_row_to_list (get_element 2 state)) (M_state_add_row_to_list (get_element 3 state))))))
     )))
                                
   
(define M_state_func_environment
  (lambda (state num)
    (cond
      ((null? num) state)
      ((eq? num (length (get_element 0 state))) state)
      ((< num (length (get_element 0 state))) (M_state_func_environment (list (cdr(get_element 0 state)) (cdr (get_element 1 state)) (get_element 2 state) (get_element 3 state)) num))
      ((> num (length (get_element 0 state))) (error "things are out of sync"))
    )))

(define M_state_create_func_layer
  (lambda (formal_params actual_params new_state)
    (cond
      ((null? formal_params) new_state)
      ((operator? (car actual_params)) new_state)
      ((boolean_operator? (car actual_params)) new_state)
      (else (M_state_create_func_layer
             (cdr formal_params)
             (cdr actual_params)
             (M_state_add_to_func_layer new_state (car formal_params) (car actual_params))
            ))
        )))

(define M_state_add_to_func_layer
  (lambda (new_state formal_param actual_param)
    (list (M_state_add_to_declared_list (get_element 0 new_state) formal_param)
          (M_state_add_to_value_list (get_element 1 new_state) actual_param)
          (get_element 2 new_state)
          (get_element 3 new_state))
    ))

(define M_state_actual_param_evaluator
  (lambda (param state return_list break try)
    (cond
      ((null? param) return_list)
      ((null? (car param)) return_list)
      
      ((number? (car param)) (M_state_actual_param_evaluator (cdr param) state (append return_list (list (car param))) break try))
      ((member? (car param) (get_element 0 state)) (M_state_actual_param_evaluator (cdr param) state (append return_list (list (M_state_lookup (car param) state break try))) break try))
      ((operator? (car param)) (cons (M_value param state break try) return_list))
      ((and (list? (car param)) (boolean_operator? (caar param))) (M_state_actual_param_evaluator (cdr param) state (append return_list (M_boolean param state break try) ) break try))
      ((list? (car param)) (M_state_actual_param_evaluator (cdr param) state (append return_list (M_state_actual_param_evaluator (car param) state return_list break try)) break try))
      ((boolean? (M_boolean_truth_finder (car param) state break try)) (append return_list (list (M_boolean_truth_finder (car param) state break try)))) ;this might need a true/false converter
      )))


(define evaluate
  (lambda (lis state return break try)
    ;(newline) (display "declared list at top of evaluate") (display (get_element 0 state))
    ;(newline) (display "value list at top of evaluate") (display (get_element 1 state))
    ;(newline) (display "lis at top of evaluate") (display lis)
    ;(newline)
    (cond
      ((null? lis) state)
      ((null? (car lis)) state)
      ((eq? 'function (caar lis)) (evaluate (cdr lis) (M_state_function_closure_maker (list (car lis)) state) return break try))
      ((and (eq? 'funcall (caar lis)) (eq? 2 (length (car lis)))) (evaluate (cdr lis) (list (get_element 0 state)
                                                                                 (M_state_sync_value_list (list (get_element 0 state)
                                                                                                                (get_element 1 (call/cc (lambda (return1) (M_value_call_function (cadar lis) '() state return1 break try))))
                                                                                                                (get_element 2 state)
                                                                                                                (get_element 3 state))) 
                                                                                 (get_element 2 state)
                                                                                 (get_element 3 state))
                                                                      return break try))
      
      ((eq? 'funcall (caar lis)) (evaluate (cdr lis) (list (get_element 0 state)
                                                                                 (M_state_sync_value_list (list (get_element 0 state)
                                                                                                                (get_element 1 (call/cc (lambda (return1) (M_value_call_function (cadar lis) (cddar lis) state return1 break try))))
                                                                                                                (get_element 2 state)
                                                                                                                (get_element 3 state))) 
                                                                                 (get_element 2 state)
                                                                                 (get_element 3 state))
                                                                      return break try))

      ((eq? 'var (caar lis)) (M_state_declaration lis state return break try))
      ((eq? '= (caar lis)) (M_state_assignment lis state return break try))
      ((eq? 'if (caar lis)) (M_state_if lis state return break try))
     
      ; when it's a "while" and the condition is true
      ((eq? 'while (caar lis))
       (evaluate (cdr lis)
                (list (get_element 0 state)
                 (M_state_sync_value_list (list (get_element 0 state) (cadr (call/cc (lambda (break) (M_state_while lis state return break try)))) (get_element 2 state) (get_element 3 state)))
                 (get_element 2 state) (get_element 3 state)) return break try))

      ((and (eq? 2 (length lis)) (and (and (list? (cadar lis)) (eq? 'return (caar lis))) (eq? 'funcall (caadar lis))))
       (call/cc (lambda (return1) (M_value_call_function (get_element 1 (cadar lis)) '() state return1 break try))))
      
      ;"return" with a "funcall"
      ((and (and (list? (cadar lis)) (eq? 'return (caar lis))) (eq? 'funcall (caadar lis)))
       (call/cc (lambda (return1) (M_value_call_function (get_element 1 (cadar lis)) (cddr(cadar lis)) state return1 break try))))
                                                                
      
      ; when it's return and a boolean, this gets it to be "true" or "false"
      ((and(eq? 'return (caar lis)) (boolean? (M_state_lookup 'return (list (get_element 0 state) (M_state_modify_value_list state 'return (M_state_return_helper (cadar lis) state break try) break try) (get_element 2 state) (get_element 3 state))break try)))
       (return (list (get_element 0 state) (M_state_modify_value_list state 'return (M_state_return_helper (cadar lis) state break try) break try) (get_element 2 state) (get_element 3 state))))

      ((and (eq? 'return (caar lis)) (number? (cadar lis)))
        (return (list (get_element 0 state) (M_state_modify_value_list state 'return (cadar lis) break try) (get_element 2 state) (get_element 3 state)))) 
      
      ((eq? 'return (caar lis))
       (return (list (get_element 0 state) (M_state_modify_value_list state 'return (M_state_return_helper (cadar lis) state break try) break try) (get_element 2 state) (get_element 3 state))))

      
      ((eq? 'begin (caar lis)) (evaluate (cdr lis) (list (get_element 0 state)
                                         (M_state_sync_value_list (list (get_element 0 state)
                                                                        (cadr (evaluate (cdar lis) (list (M_state_add_row_to_declared_list (get_element 0 state))
                                                                                                         (M_state_add_row_to_value_list (get_element 1 state))
                                                                                                         (M_state_add_row_to_list (get_element 2 state))
                                                                                                         (M_state_add_row_to_list (get_element 3 state)))
                                                                                        return break try))
                                                                        (get_element 2 state)
                                                                        (get_element 3 state)))
                                          (get_element 2 state)
                                          (get_element 3 state))
                                         return break try))

      ((eq? 'continue (caar lis)) (evaluate '() state return break try))

      ((eq? 'break (caar lis))(break (list (get_element 0 state) (M_state_sync_value_list state) (get_element 2 state) (get_element 3 state))))

      ; hit "try", "finally" doesn't exist, throw is triggered.
      ((and (and (eq? 'try (caar lis)) (null? (get_element 3 (car lis))))  (eq? 2 (length(M_state_try (get_element 1 (car lis)) state return break try))))
       (let ((x (M_state_try (get_element 1 (car lis)) state return break try)))
       (evaluate (cdr lis) (list (get_element 0 state)                
                                        (M_state_sync_value_list (list (get_element 0 state)
                                                                       (cadr (M_state_catch
                                                                              (get_element 2 (car lis))
                                                                              (list (get_element 0 state)
                                                                              (M_state_sync_value_list (list (get_element 0 (get_element 0 x))
                                                                                                             (cadar x)
                                                                                                             ; ^ this might be wrong
                                                                                                             (get_element 2 state)
                                                                                                             (get_element 3 state)))
                                                                              (get_element 2 state)
                                                                              (get_element 3 state))
                                                                              (M_value (list (list-ref x 1)) (car x) break try)
                                                                              return break try))
                                                                       (get_element 2 state)
                                                                       (get_element 3 state)))                                     
                                        (get_element 2 state)
                                        (get_element 3 state)
                                        return break try))))
      
      ; hit "try", "finally" doesn't exist, throw is not triggered.
      ((and (eq? 'try (caar lis)) (null? (get_element 3 (car lis))))
       (evaluate (cdr lis) (list (get_element 0 state)                                      
                                 (M_state_sync_value_list (list (get_element 0 state)
                                                                (cadr (M_state_try (get_element 1 (car lis)) state return break try))
                                                                (get_element 2 state)
                                                                (get_element 3 state)))       
                                        (get_element 2 state)
                                        (get_element 3 state))
                                        return break try))

       ; hit "try", "finally" does exist, throw is triggered.
      ((and (eq? 'try (caar lis)) (eq? 2 (length(M_state_try (get_element 1 (car lis)) state return break try))))
       (let ((x (M_state_try (get_element 1 (car lis)) state return break try)))
       (evaluate (cdr lis) (list (get_element 0 state)
                 (cadr (M_state_finally (cadr(get_element 3 (car lis)))
                                        (list (get_element 0 state)
                                        (M_state_sync_value_list
                                         (list (get_element 0 state)
                                               (cadr (M_state_catch (get_element 2 (car lis))
                                                                    (list (get_element 0 state)
                                                                          (M_state_sync_value_list (list (get_element 0 state)
                                                                                                         (cadar x)
                                                                                                         (get_element 2 state)
                                                                                                         (get_element 3 state)))
                                                                          (get_element 2 state)
                                                                          (get_element 3 state))
                                                                    (M_value (list (list-ref x 1)) (car x) break try)
                                                                    return break try))
                                               (get_element 2 state)
                                               (get_element 3 state)))
                                         (get_element 2 state)
                                         (get_element 3 state)
                                         return break try)
                                        return break try))
                 (get_element 2 state)
                 (get_element 3 state))
                 return break try))) 

      ; hit "try", "finally" does exist, throw is not triggered.
      ((eq? 'try (caar lis))
       (evaluate (cdr lis) (list (get_element 0 state)
                                 (cadr (M_state_finally (cadr (get_element 3 (car lis)))
                                                        (list (get_element 0 state)                                                                                               
                                                              (M_state_sync_value_list
                                                               (list (get_element 0 state)
                                                                     (cadr (M_state_try
                                                                            (get_element 1 (car lis)) state return break try))
                                                                     (get_element 2 state)
                                                                     (get_element 3 state)))
                                                              (get_element 2 state)
                                                              (get_element 3 state))
                                                              return break try))
                                 (get_element 2 state)
                                 (get_element 3 state)) return break try))

      ((eq? 'throw (caar lis)) (try (list state (cadar lis))))
      
      )))

(define M_state_declaration
  (lambda (lis state return break try)
    (cond
       ;if the first word is "var" and this sublist just has the declaration in it (ex: var x)
      ((eq? '() (cddar lis))
       (evaluate (cdr lis)
                 (list (M_state_add_to_declared_list (get_element 0 state) (cadar lis))
                       (M_state_add_to_value_list (get_element 1 state) "error")
                       (get_element 2 state)
                       (get_element 3 state))
                 return break try))
      ;if the first word is "var" and the right is a number (ex: var x = 10)
      ((number? (get_element 2 (car lis)))
       (evaluate (cdr lis)
                 (list (M_state_add_to_declared_list (get_element 0 state) (cadar lis))
                       (M_state_add_to_value_list (get_element 1 state) (get_element 2 (car lis)))
                       (get_element 2 state)
                       (get_element 3 state))
                 return break try))
      ; if the first word is var and the 3rd element is a variable. (ex: var x = y (and y is declared)) 
      ((member? (get_element 2 (car lis)) (get_element 0 state))
       (evaluate (cdr lis)
                 (list (M_state_add_to_declared_list (get_element 0 state) (cadar lis))
                       (M_state_add_to_value_list (get_element 1 state) (M_state_lookup(get_element 2 (car lis)) state))
                       (get_element 2 state)
                       (get_element 3 state))
                 return break try))
      ;if the first word is "var" and this sublist is a boolean. ex: var x = a && b
      ((boolean_operator? (car (caddar lis)))
       (evaluate (cdr lis)
                 (list (M_state_add_to_declared_list (get_element 0 state) (cadar lis))
                       (M_state_add_to_value_list (get_element 1 state) (M_boolean_tf_to_truefalse (M_boolean (caddar lis) state break try)))
                       (get_element 2 state)
                       (get_element 3 state))
                 return break try))

      ;if the first word is "var" and there is more in this sublist than just the declaration aka our first word is var and it's not of the others ex: var x = 5+7
      (else (evaluate (cdr lis)
                 (list (M_state_add_to_declared_list (get_element 0 state) (cadar lis))
                       (M_state_add_to_value_list (get_element 1 state) (M_value (caddar lis) state break try))
                       (get_element 2 state)
                       (get_element 3 state))
                 return break try))
      )))

(define M_state_assignment
  (lambda (lis state return break try)
    (cond
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a list and a boolean ex: x = a && b
      ((and (and (member? (cadar lis) (get_element 0 state)) (list? (caddar lis))) (boolean_operator? (car (caddar lis))))
       (evaluate (cdr lis) (list (get_element 0 state)
                                 (M_state_modify_value_list state (cadar lis) (M_boolean_tf_to_truefalse (M_boolean (caddar lis) state break try)) break try)
                                 (get_element 2 state)
                                 (get_element 3 state))
                                 return break try))
      
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a list and not a boolean ex: x = 5+7
      ((and (member? (cadar lis) (get_element 0 state)) (list? (caddar lis)))
       (evaluate (cdr lis) (list (get_element 0 state)
                 (M_state_modify_value_list state (cadar lis) (M_value (caddar lis) state break try) break try)
                 (get_element 2 state)
                 (get_element 3 state))
                 return break try))
      
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a number ex: x = 5
      ((and (member? (cadar lis) (get_element 0 state)) (number? (caddar lis)))
       (evaluate (cdr lis) (list (get_element 0 state)
                                 (M_state_modify_value_list state (cadar lis) (caddar lis) break try)
                                 (get_element 2 state)
                                 (get_element 3 state))
                                 return break try))
      
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a variable ex: x = y
      ((and (member? (cadar lis) (get_element 0 state)) (member? (caddar lis) (get_element 0 state)))
       (evaluate (cdr lis) (list (get_element 0 state)
                                 (M_state_modify_value_list state (cadar lis) (M_state_lookup (caddar lis) state break try) break try)
                                 (get_element 2 state)
                                 (get_element 3 state))
                                 return break try))
      ; if it's an assignment statement and gets to this line, it's not in the declared list and should fail. 
      (else (error "our version of variable not initialized"))
      )))

(define M_state_if
  (lambda (lis state return break try)
    (cond
       ; if the list starts with "if" and the condition next to it is true...then evalute it (we think this one is right) 
      ((M_boolean (cadar lis) state break try)
       (evaluate (cdr lis)
                 (let ((x (evaluate (list (get_element 2 (car lis))) state return break try)))
                 (list (car x)
                 (cadr x)
                 (get_element 2 state)
                 (get_element 3 state)))
                 return break try))
      
      ; if the condition is not true, the list has 4 elements and it starts with an "if"
      ((eq? (length (car lis)) 4)
       (evaluate (cdr lis)
                 (let ((x (evaluate (list (get_element 3 (car lis))) state return break try)))
                 (list (car x)
                 (cadr x)
                 (get_element 2 state)
                 (get_element 3 state)))
                 return break try))
      
       ; if the list starts with "if", but the condition is not true
      (else (evaluate (cdr lis) state return break try))
    )))

(define M_state_while
  (lambda (lis state return break try)
    (cond
      ((M_boolean (cadar lis) state break try)
       (let ((x (evaluate (cddar lis) state return break try)))
       (M_state_while lis (list (car x)
                                (cadr x)
                                (get_element 2 state)
                                (get_element 3 state))
                                return break try)))
      (else state)
    )))

    
(define M_state_try
  (lambda (lis state return break try)
      (call/cc (lambda (try) (evaluate lis state return break try)))
    ))

(define M_state_catch
  (lambda (lis state thrown_value return break try)
      (evaluate (get_element 2 lis)
                (list (M_state_add_to_declared_list (M_state_add_row_to_declared_list (get_element 0 state)) (car (get_element 1 lis)))
                (M_state_add_to_value_list (M_state_add_row_to_value_list (get_element 1 state)) thrown_value)
                (M_state_add_row_to_list (get_element 2 state))
                (M_state_add_row_to_list (get_element 3 state)))
                return break try)
    ))

(define M_state_finally
  (lambda (lis state return break try)
    (evaluate lis state return break try)
     ))
           
(define M_state_sync_value_list
  (lambda (state)
    (cond
      ((eq? (length (get_element 0 state)) (length (get_element 1 state))) (get_element 1 state))
      (else (M_state_sync_value_list (list (get_element 0 state)
                                           (M_state_remove_top_layer_value_list (get_element 1 state))
                                           (get_element 2 state)
                                           (get_element 3 state))))
      )))
                        


(define M_state_add_row_to_declared_list
  (lambda declared_list 
    (cons '() (car declared_list))
    ))

(define M_state_add_row_to_value_list
  (lambda value_list  
    (cons '() (car value_list))
    ))

(define M_state_add_row_to_list
  (lambda lis
    (cons '() (car lis))
    ))

(define M_state_add_to_declared_list
  (lambda (declared_list var)
    (cons (cons (box var) (car declared_list)) (cdr declared_list))
    ))

(define M_state_add_to_func_name_list
  (lambda (func_name_list var)
    (cons (cons (box var) (car func_name_list)) (cdr func_name_list))
    ))

(define M_state_add_to_func_closure_list
  (lambda (func_closure_list var)
    (cons (cons var (car func_closure_list)) (cdr func_closure_list))
    ))

(define M_state_remove_top_layer_declared_list
  (lambda (declared_list)
    (cdr declared_list)
    ))

(define M_state_remove_top_layer_value_list
  (lambda (value_list)
    (cdr value_list)
    ))

(define M_state_add_to_value_list
  (lambda (value_list val)
    (cond
      ((list? val) (error "val is a list for some reason"))
      (else (cons (cons (box val) (car value_list)) (cdr value_list)))
       )))

;if the variable is already in the declared-list, then this changes the value in the value list.
(define M_state_modify_value_list
  (lambda (state var newval break try)
    (cond
      ((null? (get_element 0 state)) (error "our version of variable not initialized"))
      ((and (list? (car (get_element 0 state))) (member? var (car (get_element 0 state))))
       (cons (M_state_modify_value_list (list (car (get_element 0 state))
                                              (car (get_element 1 state))
                                              (get_element 2 state)
                                              (get_element 3 state))
                                        var newval break try) (cdr (get_element 1 state))))
      
      ((list? (car (get_element 0 state)))
       (cons (car (get_element 1 state)) (M_state_modify_value_list (list (cdr (get_element 0 state))
                                                                          (cdr (get_element 1 state))
                                                                          (get_element 2 state)
                                                                          (get_element 3 state))
                                                                          var newval break try)))
      
     
      ((eq? var (unbox (car (get_element 0 state)))) (set-box! (car (get_element 1 state)) newval) (get_element 1 state))   
      (else (cons (car (get_element 1 state)) (M_state_modify_value_list (list (cdr (get_element 0 state))
                                                                               (cdr (get_element 1 state))
                                                                               (get_element 2 state)
                                                                               (get_element 3 state))
                                                                         var newval break try)))
    )))

(define M_state_lookup
  (lambda (var state break try)
    (cond
      ((null? (get_element 0 state)) (error "variable not found"))
      ((and (and (list? (car (get_element 0 state))) (member? var (car (get_element 0 state)))) (not(null? (car (get_element 0 state)))))
       (M_state_lookup var (list (car (get_element 0 state))
                                 (car (get_element 1 state))
                                 (get_element 2 state)
                                 (get_element 3 state)) break try))
      
      ((list? (car (get_element 0 state))) (M_state_lookup var (list (cdr (get_element 0 state))
                                                                     (cdr (get_element 1 state))
                                                                     (get_element 2 state)
                                                                     (get_element 3 state)) break try))
      
      ((eq? var (unbox (car (get_element 0 state)))) (variable_type?(unbox (car (get_element 1 state))) break try))
      (else (M_state_lookup var (list (cdr (get_element 0 state))
                                      (cdr (get_element 1 state))
                                      (get_element 2 state)
                                      (get_element 3 state)) break try))
      )))

(define M_state_func_lookup
  (lambda (var state)
    (cond
      ((null? (get_element 2 state)) (error "function not found"))
      ((and (and (list? (car (get_element 2 state))) (member? var (car (get_element 2 state)))) (not(null? (car (get_element 2 state)))))
       (M_state_func_lookup var (list (get_element 0 state)
                                 (get_element 1 state)
                                 (car (get_element 2 state))
                                 (car (get_element 3 state)))))
      
      ((list? (car (get_element 2 state))) (M_state_func_lookup var (list (get_element 0 state)
                                                                     (get_element 1 state)
                                                                     (cdr (get_element 2 state))
                                                                     (cdr (get_element 3 state)))))
      
      ((eq? var (unbox (car (get_element 2 state)))) (car (get_element 3 state)))
      (else (M_state_func_lookup var (list (get_element 0 state)
                                      (get_element 1 state)
                                      (cdr (get_element 2 state))
                                      (cdr (get_element 3 state)))))
      )))

(define M_value
  (lambda (expression state break try)
    (cond
      ((null? expression) '())
      ((number? expression) expression)
      ((number? (car expression)) (car expression))
      ((member? (get_element 0 expression) (get_element 0 state)) (M_state_lookup (get_element 0 expression) state break try))
      ((and (and (eq? (length expression) 2) (eq? (car expression) '-))) 
       (* -1 (M_value (get_element 1 expression) state break try)))
       
      ((and (and (eq? (car expression) '+) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (+ (get_element 1 expression) (get_element 2 expression)))
      
      ((and (and (eq? (car expression) '-) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (- (get_element 1 expression) (get_element 2 expression)))

     
      
      ((and (and (eq? (car expression) '*) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (* (get_element 1 expression) (get_element 2 expression)))
      
      ((and (and (eq? (car expression) '/) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (/ (get_element 1 expression) (get_element 2 expression)))
      
      ((and (and (eq? (car expression) '%) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (remainder (get_element 1 expression) (get_element 2 expression)))

      ;these are for "funcall"'s that don't have actual parameters 
      ((and (eq? 2 (length expression)) (eq? 'funcall (get_element 0 expression)))
       (M_state_lookup 'return (call/cc (lambda (return1) (M_value_call_function (get_element 1 expression) '() state return1 break try))) break try))
      
      ((and (and (list? (get_element 1 expression)) (eq? 2 (length (get_element 1 expression)))) (eq? 'funcall (car (get_element 1 expression))))
       (M_value (list (get_element 0 expression)
                      (M_state_lookup 'return (call/cc (lambda (return1) (M_value_call_function (get_element 1 (get_element 1 expression)) '() state return1 break try))) break try)
                      (get_element 2 expression)) state break try))

      ((and (and (list? (get_element 2 expression)) (eq? 2 (length (get_element 2 expression)))) (eq? 'funcall (car (get_element 2 expression))))
       (M_value (list (get_element 0 expression)
                      (get_element 1 expression)
                      (M_state_lookup 'return (call/cc (lambda (return1) (M_value_call_function (get_element 1 (get_element 2 expression)) '() state return1 break try))) break try)) state break try))

      ; the rest of these "funcall" ones are if there are actual params passed in
      ((eq? 'funcall (get_element 0 expression))
       (M_state_lookup 'return (call/cc (lambda (return1) (M_value_call_function (get_element 1 expression) (cddr expression) state return1 break try))) break try))
      
      ((and (list? (get_element 1 expression)) (eq? 'funcall (car (get_element 1 expression))))
       (M_value (list (get_element 0 expression)
                      (M_state_lookup 'return (call/cc (lambda (return1) (M_value_call_function (get_element 1 (get_element 1 expression)) (list (cddr (get_element 1 expression))) state return1 break try))) break try)
                      (get_element 2 expression)) state break try))

      ((and (list? (get_element 2 expression)) (eq? 'funcall (car (get_element 2 expression))))
       (M_value (list (get_element 0 expression)
                      (get_element 1 expression)
                      (M_state_lookup 'return (call/cc (lambda (return1) (M_value_call_function (get_element 1 (get_element 2 expression)) (list (cddr (get_element 2 expression))) state return1 break try))) break try)) state break try))
      
      ((list? (get_element 1 expression))
       (M_value (list (get_element 0 expression)
                             (M_value (get_element 1 expression) state break try) (get_element 2 expression)) state break try))

      ((list? (get_element 2 expression))
       (M_value (list (get_element 0 expression)
                              (get_element 1 expression)  (M_value (get_element 2 expression) state break try)) state break try))

      ((member? (get_element 1 expression) (get_element 0 state))
       (M_value (list (get_element 0 expression)
                             (M_state_lookup (get_element 1 expression) state break try) (get_element 2 expression)) state break try))

      ((member? (get_element 2 expression) (get_element 0 state))
       (M_value (list (get_element 0 expression)
                              (get_element 1 expression)  (M_state_lookup (get_element 2 expression) state break try)) state break try))
      )))



(define M_boolean
  (lambda (expression state break try)
    (cond
      ((null? expression) false)
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((and (and (list? (get_element 1 expression)) (eq? (car expression) '!)) (boolean_operator? (car (get_element 1 expression))))
       (M_boolean (list (get_element 0 expression) (M_boolean (get_element 1 expression) state break try)) state break try))
      ((eq? (car expression) '!) (M_boolean_tf_to_hashtags (not (M_boolean_truth_finder (get_element 1 expression) state break try))))
      
      ((boolean? expression) expression)
     
      ;Checks if left side is a boolean equation or a integer and calcs it
      ((eq? (get_element 1 expression) 'true) (M_boolean (list (get_element 0 expression) #t (get_element 2 expression)) state break try))
      ((eq? (get_element 1 expression) 'false) (M_boolean (list (get_element 0 expression) #f (get_element 2 expression)) state break try))
      ((and (list? (get_element 1 expression)) (boolean_operator? (car (get_element 1 expression))))
       (M_boolean (list (get_element 0 expression) (M_boolean (get_element 1 expression) state break try) (get_element 2 expression)) state break try))
      ((list? (get_element 1 expression))
       (M_boolean (list (get_element 0 expression) (M_value (get_element 1 expression) state break try) (get_element 2 expression)) state break try))

      ; as above but for right side
      ((eq? (get_element 2 expression) 'true) (M_boolean (list (get_element 0 expression) (get_element 1 expression) #t) state break try))
      ((eq? (get_element 2 expression) 'false) (M_boolean (list (get_element 0 expression) (get_element 1 expression) #f) state break try))
      ((and (list? (get_element 2 expression)) (boolean_operator? (car (get_element 2 expression))))
       (M_boolean (list (get_element 0 expression) (get_element 1 expression) (M_boolean (get_element 2 expression) state break try)) state break try))
      ((list? (get_element 2 expression))
       (M_boolean (list (get_element 0 expression) (get_element 1 expression) (M_value (get_element 2 expression) state break try)) state break try))

      
      ((eq? (car expression) '&&)
       (M_boolean_tf_to_hashtags (and (M_boolean_truth_finder (get_element 1 expression) state break try) (M_boolean_truth_finder (get_element 2 expression) state break try))))
      ((eq? (car expression) '||)
       (M_boolean_tf_to_hashtags (or (M_boolean_truth_finder (get_element 1 expression) state break try) (M_boolean_truth_finder (get_element 2 expression) state break try))))
      
      ; if the left or right element is not a number, look up the value of the variable. 
      ((not(number? (get_element 1 expression)))
       (M_boolean (list (get_element 0 expression) (M_state_lookup (get_element 1 expression) state break try) (get_element 2 expression)) state break try));added on plane
      ((not(number? (get_element 2 expression)))
       (M_boolean (list (get_element 0 expression) (get_element 1 expression) (M_state_lookup (get_element 2 expression) state break try)) state break try));added on plane


      ((eq? (car expression) '==) (M_boolean_equal (get_element 1 expression) (get_element 2 expression)))
      ((eq? (car expression) '!=) (not (M_boolean_equal (get_element 1 expression) (get_element 2 expression))))
      ((eq? (car expression) '<) (< (get_element 1 expression) (get_element 2 expression)))
      ((eq? (car expression) '>) (> (get_element 1 expression) (get_element 2 expression)))
      ((eq? (car expression) '<=) (<= (get_element 1 expression) (get_element 2 expression)))
      ((eq? (car expression) '>=) (>= (get_element 1 expression) (get_element 2 expression)))

      
      
    )))

(define M_boolean_truth_finder
  (lambda (var state break try)
    (cond
      ((null? var) #f)
      ((boolean? var) var)
      ((member? var (get_element 0 state)) (M_boolean_tf_to_hashtags(M_state_lookup var state break try)))
      ((eq? var 'true) #t)
      ((eq? var 'false) #f)
      (else (error "our version of variable not initialized"))
      )))

(define M_boolean_equal
  (lambda (left right)
    (cond
      ((= left right) #t)
      (else #f))))


    
(define M_boolean_truefalse_converter
  (lambda (var)
    (cond
      ((eq? var 'true) #t)
      ((eq? var #t) 'true)
      ((eq? var 'false) #f)
      ((eq? var #f) 'false)
      )))

(define M_boolean_tf_to_truefalse
  (lambda (var)
    (cond
      ((eq? var 'true) 'true)
      ((eq? var #t) 'true)
      ((eq? var 'false) 'false)
      ((eq? var #f) 'false)
      )))

(define M_boolean_tf_to_hashtags
  (lambda (var)
    (cond
      ((eq? var 'true) #t)
      ((eq? var #t) #t)
      ((eq? var 'false) #f)
      ((eq? var #f) #f)
      )))


; variable_type tells us if the variable is declared or not. If it's 'true' or 'false' it converts it to a hashtag
(define variable_type?
  (lambda (var break try)
    (cond
      ((null? var) '())
      ((and (list? var) (null? (car var))) '())
      ((and (list? var) (number? (car var))) (car var))
      ((number? var) var)
      ((and (list? var) (boolean? (car var))) (car var))
      ((boolean? var) var)
      ((and (list? var) (or (eq? 'true (car var))) (eq? 'false (car var))) (M_boolean_tf_to_hashtags (car var)))
      ((or (eq? 'true var) (eq? 'false var)) (M_boolean_tf_to_hashtags var))
      (else (display var) (error "our version of variable not initialized"))
      )))

(define member?
  (lambda (x lis)
    (cond
      ((null? lis) #f)
      ((list? (car lis)) (or (member? x (car lis)) (member? x (cdr lis))))
      ((and (box? (car lis)) (eq? x (unbox (car lis)))) #t) 
      ((eq? x (car lis)) #t)
      (else (member? x (cdr lis))))))

(define boolean_operator?
  (lambda (var)
    (cond
      ((member? var (list '== '!= '< '> '<= '>= '&& '|| '!)) #t)
      (else #f)
      )))

(define operator?
  (lambda (var)
    (cond
      ((member? var (list '+ '- '* '/ '%)) #t)
      (else #f)
      )))

; if what we have is just a list
; element 0 = car
; element 1 = cadr
; element 2 = caddr
; element 3 = cadddr

(define get_element
  (lambda (index lis);first item = 0
    (cond
      ((null? lis) '())
      ((eq? index 0) (car lis))
      ((eq? index 1) (cadr lis))
      ((eq? index 2) (caddr lis))
      ((eq? index 3) (cadddr lis))
      (else (get_element (- index 1) (cdr lis)))
      )))

(define M_state_return_helper
  (lambda (var state break try)
    (cond
      ((null? var) 'null)
      ((number? var) var)
      ((member? var (get_element 0 state)) (M_state_lookup var state break try))  
      ((and (list? var) (boolean_operator? (car var))) (M_boolean var state break try))
      ((list? var) (M_value var state break try))
      ((eq? 'false var) var)
      ((eq? 'true var) var)
      (else (error "invalid return"))
       )))

(define tests
  (lambda x
      (cond
        ((not (eq? (interpret "Unit Tests/fileToParseTest2-1.txt") 20)) (error "Test 2-1 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest2-2.txt") 164)) (error "Test 2-2 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest2-3.txt") 32)) (error "Test 2-3 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest2-4.txt") 2)) (error "Test 2-4 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest2-6.txt") 25)) (error "Test 2-6 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest2-7.txt") 21)) (error "Test 2-7 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest2-8.txt") 6)) (error "Test 2-8 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest2-9.txt") -1)) (error "Test 2-9 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest2-10.txt") 789)) (error "Test 2-10 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest2-14.txt") 12)) (error "Test 2-14 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest2-15.txt") 125)) (error "Test 2-15 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest2-16.txt") 110)) (error "Test 2-16 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest2-17.txt") 2000400)) (error "Test 2-17 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest2-18.txt") 101)) (error "Test 2-18 failed"))
        (display "all tests passed")
        )))
;(tests)


(define tests3
  (lambda x
      (cond
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-1.txt") 10)) (error "Test 3-1 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-2.txt") 14)) (error "Test 3-2 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-3.txt") 45)) (error "Test 3-3 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-4.txt") 55)) (error "Test 3-4 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-5.txt") 1)) (error "Test 3-5 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-6.txt") 115)) (error "Test 3-6 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-7.txt") 'true)) (error "Test 3-7 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-8.txt") 20)) (error "Test 3-8 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-9.txt") 24)) (error "Test 3-9 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-10.txt") 2)) (error "Test 3-10 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-11.txt") 35)) (error "Test 3-11 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-13.txt") 90)) (error "Test 3-13 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-14.txt") 69)) (error "Test 3-14 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-15.txt") 87)) (error "Test 3-15 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-18.txt") 125)) (error "Test 3-18 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-19.txt") 100)) (error "Test 3-19 failed"))
       ; ((not (eq? (interpret "Unit Tests/fileToParseTest3-20.txt") 2000400)) (error "Test 2-18 failed"))
        
        (display "all tests passed")
        )))
;(tests3)

(interpret "Unit Tests/fileToParseTest4-1.txt" "A")



;(interpret "Unit Tests/fileToParseTest3-20.txt")
;(interpret "Unit Tests/fileToParseTest3-5.txt")
;(interpret "Unit Tests/fileToParseTest3-6.txt")

; These below are unit tests of Part 1
; Note that the ones that end in "true" or "false" aren't returning #t correctly

#|
(eq? (interpret "Unit Tests/fileToParseTest1-1.txt") 150)
(eq? (interpret "Unit Tests/fileToParseTest1-2.txt") -4)
(eq? (interpret "Unit Tests/fileToParseTest1-3.txt") 10)
(eq? (interpret "Unit Tests/fileToParseTest1-4.txt") 16)
(eq? (interpret "Unit Tests/fileToParseTest1-5.txt") 220)
(eq? (interpret "Unit Tests/fileToParseTest1-6.txt") 5)
(eq? (interpret "Unit Tests/fileToParseTest1-7.txt") 6)
(eq? (interpret "Unit Tests/fileToParseTest1-8.txt") 10)
(eq? (interpret "Unit Tests/fileToParseTest1-9.txt") 5)
(eq? (interpret "Unit Tests/fileToParseTest1-10.txt") -39)
;test 11,12,13 should fail
;(interpret "Unit Tests/fileToParseTest1-11.txt")
;(interpret "Unit Tests/fileToParseTest1-12.txt")
;(interpret "Unit Tests/fileToParseTest1-13.txt")

(eq? (interpret "Unit Tests/fileToParseTest1-14.txt") 30)
(eq? (interpret "Unit Tests/fileToParseTest1-15.txt") "'true")
(eq? (interpret "Unit Tests/fileToParseTest1-16.txt") 100)
(eq? (interpret "Unit Tests/fileToParseTest1-17.txt") "'false")
(eq? (interpret "Unit Tests/fileToParseTest1-18.txt") "'true")
(eq? (interpret "Unit Tests/fileToParseTest1-19.txt") 128)
(eq? (interpret "Unit Tests/fileToParseTest1-20.txt") 12)
|#

