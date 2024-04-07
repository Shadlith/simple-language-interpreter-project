#lang racket

; Group 8 - Suvaion Das, Remy Niman, Faraz Mamaghani
; Simple Language Interpreter

;(require "sectionParser.rkt")
(require "functionParser.rkt")

(define parse
  (lambda (filename)
    (parser filename)
    ))

;(((#&return)) ((#&null)) ((#&main) (#&fib)) (((() ((return (funcall fib 10))) 1)) (((a) ((if (== a 0) (return 0) (if (== a 1) (return 1) (return (+ (funcall fib (- a 1)) (funcall fib (- a 2))))))) 1))))
;'(((#&r #&y #&x #&return)) ((#&0 #&10 #&1 #&null)) ((#&main)) (((() ((while (< x y) (begin (= r (+ r x)) (= x (+ x 1)))) (return r)) 1))))

;code to run on: (interpret "fileToParse.txt")
(define interpret
  (lambda (filename)
    (define parsed (parse filename))
    (print parsed);just to see what it outputs
    (newline)
    (let ((x (M_state_closure_maker parsed (list (list(list(box 'return))) (list(list(box 'null))) '(()) '(())))))
    (display "closure: ") (print x)
    (answer_converter (M_state_lookup 'return (call/cc (lambda (return1) (M_value_call_function 'main '(())
                           (list (get_element 0 x) (get_element 1 x) (get_element 2 x) (get_element 3 x)) return1))))))
    
    ; We need other functions to deal with function calls in them. Test 4 fails right now b/c return doesn't know what to do with a function. 
    ))


(define M_state_closure_maker
  (lambda (lis state)
    (cond
      ((null? lis) state)
      
      ((eq? 'function (caar lis))
       (M_state_closure_maker (cdr lis)
                              (list (get_element 0 state) (get_element 1 state)
                              (M_state_add_to_func_name_list (get_element 2 state) (cadar lis))
                              (M_state_add_to_func_closure_list (get_element 3 state)
                                                                (list(get_element 2 (car lis)) (get_element 3 (car lis)) (length (get_element 0 state)))))))
                              
     ((or (eq? 'var (caar lis)) (eq? '= (caar lis))) (evaluate lis state '() '()' ()))
      )))

(define answer_converter
  (lambda (var)
    (cond
      ((and (boolean? var) var) 'true)
      ((boolean? var) 'false)
      (else var))))

(define M_value_call_function
 (lambda (func_name actual_params state return)
   (cond 
     ((null? (get_element 2 state)) (error "function not in func_name list"))
     ;((list? (car (get_element 2 state))) (call/cc (lambda (return1) (M_value_call_function func_name actual_params (list (get_element 0 state)
                                                                                            ;   (get_element 1 state)
                                                                                              ; (car (get_element 2 state))
                                                                                               ;(car (get_element 3 state)))return1))))

      
     ((member? func_name (get_element 2 state)) (let ((x1 (M_state_func_lookup func_name state))) (call/cc (lambda (return1) (evaluate (get_element 1 x1)
                                                                    (list (car (M_state_func_environment_shell (get_element 2 x1)
                                                                                                               (get_element 0 x1)
                                                                                                               (cadr (M_state_actual_param_evaluator actual_params state '())) (car (M_state_actual_param_evaluator actual_params state '()))))
                                                                          (cadr (M_state_func_environment_shell (get_element 2 x1)
                                                                                                               (get_element 0 x1)
                                                                                                               (cadr (M_state_actual_param_evaluator actual_params state '())) (car (M_state_actual_param_evaluator actual_params state '()))))
                                                                          (M_state_add_row_to_list (get_element 2 state))
                                                                          (M_state_add_row_to_list (get_element 3 state)))
                                                                    return1 '() '()))))
     )
                                                                          

     #|
     ((eq? func_name (unbox (car (get_element 2 state) )))
      (call/cc (lambda (return1) (evaluate
       (get_element 1 (car (get_element 3 state)))
       (list (car (M_state_func_environment_shell (get_element 2 (car (get_element 3 state)))
                                                  (get_element 0 (car (get_element 3 state)))
                                                  (M_state_actual_param_evaluator actual_params state '()) state))
       (cadr (M_state_func_environment_shell (get_element 2 (car (get_element 3 state)))
                                             (get_element 0 (car (get_element 3 state)))
                                             (M_state_actual_param_evaluator actual_params state '()) state))
       (get_element 2 state) (get_element 3 state)) return1 '() '()))))
     |#
     
     (else (call/cc (lambda (return1) (M_value_call_function func_name actual_params
                                  (list (get_element 0 state) (get_element 1 state) (cdr (get_element 2 state)) (cdr (get_element 3 state))) return1))))
    )))

(define M_state_func_environment_shell
  (lambda (num formal_params actual_params state)
    (cond
      ((not (eq? (length actual_params) (length formal_params))) (error "formal params and actual params are not the same length"))
      (else (M_state_create_func_layer formal_params actual_params
                                (list (M_state_add_row_to_declared_list (car (M_state_func_environment state num)))
                                (M_state_add_row_to_value_list (cadr (M_state_func_environment state num)))
                                (M_state_add_row_to_list (get_element 2 state)) (M_state_add_row_to_list (get_element 3 state)))))
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
  (lambda (param state return_list)
    (cond
      ((null? param) (list state return_list))
      ((null? (car param)) (list state return_list))
      
      ((number? (car param)) (M_state_actual_param_evaluator (cdr param) state (append return_list (list (car param)))))
      ((member? (car param) (get_element 0 state)) (M_state_actual_param_evaluator (cdr param) state (append return_list (list (M_state_lookup (car param) state)))))
      
      ;FIGURE THIS OUT FOR TEST 9 ((eq? 'funcall (car param)) (M_value_call_function (cadr param) param declared_list value_list func_name_list func_closure_list))
      ((operator? (car param)) (cons (cadr (M_value param state)) return_list))
      ((and (list? (car param)) (boolean_operator? (caar param))) (M_state_actual_param_evaluator (cdr param) state (append return_list (M_boolean param state) )))
      ((list? (car param)) (M_state_actual_param_evaluator (cdr param) state (append return_list (M_state_actual_param_evaluator (car param) state return_list))))
      ((boolean? (M_boolean_truth_finder (car param) state)) (append return_list (list (M_boolean_truth_finder (car param) state)))) ;this might need a true/false converter
      )))
   
(define M_state_nest_func
  (lambda (state)
    (list (get_element 0 state)
          (get_element 1 state)
          (M_state_add_row_to_list (get_element 2 state))
          (M_state_add_row_to_list (get_element 3 state)))))


(define evaluate
  (lambda (lis state return break try)
    ;(newline) (display "declared list at top of evaluate: ") (display (get_element 0 state))
    ;(newline) (display "value list at top of evaluate: ") (display (get_element 1 state))
    ;(newline) (display "lis at top of evaluate") (display lis)
    (cond
      ((null? lis) state)
      ((null? (car lis)) state)
      ((eq? 'function (caar lis)) (evaluate (cdr lis) (M_state_closure_maker (list (car lis)) state) return break try))
      ((and (eq? 'funcall (caar lis)) (eq? 2 (length (car lis)))) (evaluate (cdr lis) (list (get_element 0 state)
                                                                                 (M_state_sync_value_list (list (get_element 0 state)
                                                                                                                (get_element 1 (call/cc (lambda (return1) (M_value_call_function (cadar lis) '() state return1))))
                                                                                                                (get_element 2 state)
                                                                                                                (get_element 3 state))) 
                                                                                 (get_element 2 state)
                                                                                 (get_element 3 state))
                                                                      return break try))
      
      ((eq? 'funcall (caar lis)) (evaluate (cdr lis) (list (get_element 0 state)
                                                                                 (M_state_sync_value_list (list (get_element 0 state)
                                                                                                                (get_element 1 (call/cc (lambda (return1) (M_value_call_function (cadar lis) (cddar lis) state return1))))
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

      ;"return" with a "funcall" with no parameters
      ((and (eq? 2 (length lis)) (and (and (list? (cadar lis)) (eq? 'return (caar lis))) (eq? 'funcall (caadar lis))))
       (call/cc (lambda (return1) (M_state_remove_top_layer (M_value_call_function (get_element 1 (cadar lis)) '() state return1)))))
      
      ;"return" with a "funcall" with parameters
      ((and (and (list? (cadar lis)) (eq? 'return (caar lis))) (eq? 'funcall (caadar lis)))
        (call/cc (lambda (return1) (M_state_remove_top_layer (M_value_call_function (get_element 1 (cadar lis)) (cddr(cadar lis)) state return1)))))
                                                                
      
      ; when it's return and a boolean, this gets it to be "true" or "false"
      ((and(eq? 'return (caar lis)) (boolean? (M_state_lookup 'return (list (get_element 0 state) (M_state_modify_value_list state 'return (M_state_return_helper (cadar lis) state)) (get_element 2 state) (get_element 3 state)))))
       (return (list (get_element 0 state) (M_state_modify_value_list state 'return (M_state_return_helper (cadar lis) state)) (get_element 2 state) (get_element 3 state))))

      ((and (eq? 'return (caar lis)) (number? (cadar lis)))
        (return (list (get_element 0 state) (M_state_modify_value_list state 'return (cadar lis)) (get_element 2 state) (get_element 3 state)))) 
      
      ((eq? 'return (caar lis))
       (return (list (get_element 0 state) (M_state_modify_value_list state 'return (M_state_return_helper (cadar lis) state)) (get_element 2 state) (get_element 3 state))))

      
      ((eq? 'begin (caar lis)) (evaluate (cdr lis) (list (get_element 0 state)
                                         (M_state_sync_value_list (list (get_element 0 state)
                                                                        (cadr (evaluate (cdar lis) (list (M_state_add_row_to_declared_list (get_element 0 state))
                                                                                                         (M_state_add_row_to_value_list (get_element 1 state))
                                                                                                         (get_element 2 state)
                                                                                                         (get_element 3 state))
                                                                                        return break try))
                                                                        (get_element 2 state)
                                                                        (get_element 3 state)))
                                          (get_element 2 state)
                                          (get_element 3 state))
                                         return break try))

      ((eq? 'continue (caar lis)) (evaluate '() state return break try))

      ((eq? 'break (caar lis))(break (list (get_element 0 state) (M_state_sync_value_list state) (get_element 2 state) (get_element 3 state))))

      ; hit "try", "finally" doesn't exist, throw is triggered.
      ((and (and (eq? 'try (caar lis)) (null? (get_element 3 (car lis))))  (eq? 3 (length(M_state_try (get_element 1 (car lis)) state return break try))))
       (evaluate (cdr lis) (list (get_element 0 state)                
                                        (M_state_sync_value_list (list (get_element 0 state)
                                                                       (cadr (M_state_catch
                                                                              (get_element 2 (car lis))
                                                                              (list (get_element 0 state)
                                                                              (M_state_sync_value_list (list (get_element 0 state) (cadr (M_state_try (get_element 1 (car lis)) state return break try)) (get_element 2 state) (get_element 3 state)))
                                                                              (get_element 2 state)
                                                                              (get_element 3 state))
                                                                              (caddr (M_state_try (get_element 1 (car lis)) state return break try))
                                                                              return break try))
                                                                       (get_element 2 state)
                                                                       (get_element 3 state)))                                     
                                        (get_element 2 state)
                                        (get_element 3 state)
                                        return break try)))
      
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
       (evaluate (cdr lis) (list (get_element 0 state)
                 (cadr (M_state_finally (cadr(get_element 3 (car lis)))
                                        (list (get_element 0 state)
                                        (M_state_sync_value_list
                                         (list (get_element 0 state)
                                               (cadr (M_state_catch (get_element 2 (car lis))
                                                                    (list (get_element 0 state)
                                                                          (M_state_sync_value_list (list (get_element 0 state)
                                                                                                         (cadr (M_state_try (get_element 1 (car lis)) state return break try))
                                                                                                         (get_element 2 state)
                                                                                                         (get_element 3 state)))
                                                                          (get_element 2 state)
                                                                          (get_element 3 state))
                                                                    (cadr (M_state_try (get_element 1 (car lis)) state return break try))
                                                                    ; ^ this may be wrong
                                                                    return break try))
                                               (get_element 2 state)
                                               (get_element 3 state)))
                                         (get_element 2 state)
                                         (get_element 3 state)
                                         return break try)
                                        return break try))
                 (get_element 2 state)
                 (get_element 3 state)
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
                       (M_state_add_to_value_list (get_element 1 state) (M_boolean_tf_to_truefalse (M_boolean (caddar lis) state)))
                       (get_element 2 state)
                       (get_element 3 state))
                 return break try))

      ;((eq? 'funcall (car (get_element 2

      ;if the first word is "var" and there is more in this sublist than just the declaration aka our first word is var and it's not of the others ex: var x = 5+7
      (else (evaluate (cdr lis)
                 (list (M_state_add_to_declared_list (get_element 0 state) (cadar lis))
                       (M_state_add_to_value_list (get_element 1 state) (cadr (M_value (caddar lis) state)))
                       (get_element 2 state)
                       (get_element 3 state))
                 return break try))
      )))

; 4-4-24: This is where we stopped teh state conversion 

(define M_state_assignment
  (lambda (lis state return break try)
    (cond
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a list and a boolean ex: x = a && b
      ((and (and (member? (cadar lis) (get_element 0 state)) (list? (caddar lis))) (boolean_operator? (car (caddar lis))))
       (evaluate (cdr lis) (list (get_element 0 state)
                                 (M_state_modify_value_list state (cadar lis) (M_boolean_tf_to_truefalse (M_boolean (caddar lis) state)))
                                 (get_element 2 state)
                                 (get_element 3 state))
                                 return break try))
      
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a list and not a boolean ex: x = 5+7
      ((and (member? (cadar lis) (get_element 0 state)) (list? (caddar lis)))
       (evaluate (cdr lis) (list (get_element 0 state)
                 (M_state_modify_value_list state (cadar lis) (cadr (M_value (caddar lis) state)))
                 (get_element 2 state)
                 (get_element 3 state))
                 return break try))
      
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a number ex: x = 5
      ((and (member? (cadar lis) (get_element 0 state)) (number? (caddar lis)))
       (evaluate (cdr lis) (list (get_element 0 state)
                                 (M_state_modify_value_list state (cadar lis) (caddar lis))
                                 (get_element 2 state)
                                 (get_element 3 state))
                                 return break try))
      
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a variable ex: x = y
      ((and (member? (cadar lis) (get_element 0 state)) (member? (caddar lis) (get_element 0 state)))
       (evaluate (cdr lis) (list (get_element 0 state)
                                 (M_state_modify_value_list state (cadar lis) (M_state_lookup (caddar lis) state))
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
      ((M_boolean (cadar lis) state)
       (evaluate (cdr lis)
                 (list (car (evaluate (list (get_element 2 (car lis))) state return break try))
                 (cadr (evaluate (list (get_element 2 (car lis))) state return break try))
                 (get_element 2 state)
                 (get_element 3 state))
                 return break try))
      
      ; if the condition is not true, the list has 4 elements and it starts with an "if"
      ((eq? (length (car lis)) 4)
       (evaluate (cdr lis)
                 (list (car (evaluate (list (get_element 3 (car lis))) state return break try))
                 (cadr (evaluate (list (get_element 3 (car lis))) state return break try))
                 (get_element 2 state)
                 (get_element 3 state))
                 return break try))
      
       ; if the list starts with "if", but the condition is not true
      (else (evaluate (cdr lis) state return break try))
    )))

(define M_state_while
  (lambda (lis state return break try)
    (cond
      ((M_boolean (cadar lis) state)
       (M_state_while lis (list (car (evaluate (cddar lis) state return break try))
                                (cadr (evaluate (cddar lis) state return break try))
                                (get_element 2 state)
                                (get_element 3 state))
                                return break try))
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

(define M_state_remove_top_layer
  (lambda (state)
    (list (cdr (get_element 0 state)) (cdr (get_element 1 state)) (cdr (get_element 2 state)) (cdr (get_element 3 state)))
    ))

(define M_state_remove_top_layer_value_list
  (lambda (value_list)
    (cdr value_list)
    ))

(define M_state_add_to_value_list
  (lambda (value_list val)
    (cond
      ;((not(or (or (number? val) (eq? 'true val)) (eq? 'false val))) (error "our version of variable not initialized"))
      ((list? val) (error "val is a list for some reason"))
      (else (cons (cons (box val) (car value_list)) (cdr value_list)))
       )))

;if the variable is already in the declared-list, then this changes the value in the value list.
(define M_state_modify_value_list
  (lambda (state var newval)
    (cond
      ((null? (get_element 0 state)) (error "our version of variable not initialized"))
      ((and (list? (car (get_element 0 state))) (member? var (car (get_element 0 state))))
       (cons (M_state_modify_value_list (list (car (get_element 0 state))
                                              (car (get_element 1 state))
                                              (get_element 2 state)
                                              (get_element 3 state))
                                        var newval) (cdr (get_element 1 state))))
      
      ((list? (car (get_element 0 state)))
       (cons (car (get_element 1 state)) (M_state_modify_value_list (list (cdr (get_element 0 state))
                                                                          (cdr (get_element 1 state))
                                                                          (get_element 2 state)
                                                                          (get_element 3 state))
                                                                          var newval)))
      
      ; 3-12: This eq line is where we have an issue.....both don't work
      ((eq? var (unbox (car (get_element 0 state)))) (cons (box newval) (cdr (get_element 1 state))))
      ; the set-box! works.....except for the last test of Part 1.....so, we're not going to do it (for now)
      ;((eq? var (unbox (car declared_list))) (begin (set-box! (car value_list) newval) value_list))
      (else (cons (car (get_element 1 state)) (M_state_modify_value_list (list (cdr (get_element 0 state))
                                                                               (cdr (get_element 1 state))
                                                                               (get_element 2 state)
                                                                               (get_element 3 state))
                                                                         var newval)))
    )))

(define M_state_lookup
  (lambda (var state)
    (cond
      ((null? (get_element 0 state)) (error "variable not found"))
      ((and (and (list? (car (get_element 0 state))) (member? var (car (get_element 0 state)))) (not(null? (car (get_element 0 state)))))
       (M_state_lookup var (list (car (get_element 0 state))
                                 (car (get_element 1 state))
                                 (get_element 2 state)
                                 (get_element 3 state))))
      
      ((list? (car (get_element 0 state))) (M_state_lookup var (list (cdr (get_element 0 state))
                                                                     (cdr (get_element 1 state))
                                                                     (get_element 2 state)
                                                                     (get_element 3 state))))
      
      ((eq? var (unbox (car (get_element 0 state)))) (variable_type?(unbox (car (get_element 1 state)))))
      (else (M_state_lookup var (list (cdr (get_element 0 state))
                                      (cdr (get_element 1 state))
                                      (get_element 2 state)
                                      (get_element 3 state))))
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
  (lambda (expression state)
    (cond
      ((number? expression) (list state expression)) ;changing M_value to return a list of the state and expression
      ((and (and (eq? (length expression) 2) (eq? (car expression) '-))) 
       (list state (M_value_mult -1 (cadr (M_value (get_element 1 expression) state)))))
       
      ((and (and (eq? (car expression) '+) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (list state (M_value_add (get_element 1 expression) (get_element 2 expression))))
      
      ((and (and (eq? (car expression) '-) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (list state (M_value_sub (get_element 1 expression) (get_element 2 expression))))

     
      
      ((and (and (eq? (car expression) '*) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (list state (M_value_mult (get_element 1 expression) (get_element 2 expression))))
      
      ((and (and (eq? (car expression) '/) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (list state (M_value_div (get_element 1 expression) (get_element 2 expression))))
      
      ((and (and (eq? (car expression) '%) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (list state (M_value_mod (get_element 1 expression) (get_element 2 expression))))

      ;these are for "funcall"'s that don't have actual parameters 
      ((and (eq? 2 (length expression)) (eq? 'funcall (get_element 0 expression)))
       (let ((x (call/cc (lambda (return1) (M_value_call_function (get_element 1 expression) '() state return1)))))
         (list (M_state_remove_top_layer x) (M_state_lookup 'return x))))
         
      ((and (and (list? (get_element 1 expression)) (eq? 2 (length (get_element 1 expression)))) (eq? 'funcall (car (get_element 1 expression))))
       (let ((x (call/cc (lambda (return1) (M_value_call_function (get_element 1 (get_element 1 expression)) '() state return1)))))
         (M_value (list (get_element 0 expression)
                      (M_state_lookup 'return x)
                      (get_element 2 expression)) (M_state_remove_top_layer x))))

      ((and (and (list? (get_element 2 expression)) (eq? 2 (length (get_element 2 expression)))) (eq? 'funcall (car (get_element 2 expression))))
       (let ((x ((call/cc (lambda (return1) (M_value_call_function (get_element 1 (get_element 2 expression)) '() state return1))))))
         (M_value (list (get_element 0 expression)
                      (get_element 1 expression)
                      (M_state_lookup 'return x)) (M_state_remove_top_layer x))))

      ; the rest of these "funcall" ones are if there are actual params passed in
      ((eq? 'funcall (get_element 0 expression))
       (let ((x (call/cc (lambda (return1) (M_value_call_function (get_element 1 expression) (cddr expression) state return1)))))
         (list (M_state_remove_top_layer x) (M_state_lookup 'return x))))

      ((and (list? (get_element 1 expression)) (eq? 'funcall (car (get_element 1 expression))))
       (let ((x (call/cc (lambda (return1) (M_value_call_function (get_element 1 (get_element 1 expression)) (list (get_element 2 (get_element 1 expression))) state return1)))))
         (M_value (list (get_element 0 expression)
                      (M_state_lookup 'return x)
                      (get_element 2 expression)) (M_state_remove_top_layer x))))

      ((and (list? (get_element 2 expression)) (eq? 'funcall (car (get_element 2 expression))))
       (let ((x (call/cc (lambda (return1) (M_value_call_function (get_element 1 (get_element 2 expression)) (list (get_element 2 (get_element 2 expression))) state return1)))))
         (M_value (list (get_element 0 expression)
                      (get_element 1 expression)
                      (M_state_lookup 'return x)) (M_state_remove_top_layer x))))
      
      ((list? (get_element 1 expression))
       (let ((x (M_value (get_element 1 expression) state)))
         (M_value (list (get_element 0 expression)
                        (cadr x) (get_element 2 expression)) (M_state_remove_top_layer (car x)))))

      ((list? (get_element 2 expression))
       (let ((x (M_value (get_element 2 expression) state)))
       (M_value (list (get_element 0 expression)
                              (get_element 1 expression)  (cadr x)) (M_state_remove_top_layer (car x)))))

      ((member? (get_element 1 expression) (get_element 0 state))
       (M_value (list (get_element 0 expression)
                             (M_state_lookup (get_element 1 expression) state) (get_element 2 expression)) state))

      ((member? (get_element 2 expression) (get_element 0 state))
       (M_value (list (get_element 0 expression)
                              (get_element 1 expression)  (M_state_lookup (get_element 2 expression) state)) state))
      )))


(define M_value_add
  (lambda (left right)
    (cond
      ((or (null? left) (null? right)) 0)
      (else (+ left right))
    )))

(define M_value_sub
  (lambda (left right)
    (cond
      ((or (null? left) (null? right)) 0)
      (else (- left right))
    )))

(define M_value_mult
  (lambda (left right)
    (cond
      ((or (null? left) (null? right)) 0)
      (else (* left right))
    )))

(define M_value_div
  (lambda (left right)
    (cond
      ((or (null? left) (null? right)) 0)
      (else (quotient left right))
    )))

(define M_value_mod
  (lambda (left right)
    (cond
      ((or (null? left) (null? right)) 0)
      (else (remainder left right))
    )))


(define M_boolean
  (lambda (expression state)
    (cond
      ((null? expression) false)
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((and (and (list? (get_element 1 expression)) (eq? (car expression) '!)) (boolean_operator? (car (get_element 1 expression))))
       (M_boolean (list (get_element 0 expression) (M_boolean (get_element 1 expression) state)) state))
      ((eq? (car expression) '!) (M_boolean_tf_to_hashtags (not (M_boolean_truth_finder (get_element 1 expression) state))))
      
      ((boolean? expression) expression)
     
      ;Checks if left side is a boolean equation or a integer and calcs it
      ((eq? (get_element 1 expression) 'true) (M_boolean (list (get_element 0 expression) #t (get_element 2 expression)) state))
      ((eq? (get_element 1 expression) 'false) (M_boolean (list (get_element 0 expression) #f (get_element 2 expression)) state))
      ((and (list? (get_element 1 expression)) (boolean_operator? (car (get_element 1 expression))))
       (M_boolean (list (get_element 0 expression) (M_boolean (get_element 1 expression) state) (get_element 2 expression)) state))
      ((list? (get_element 1 expression))
       (M_boolean (list (get_element 0 expression) (cadr (M_value (get_element 1 expression) state)) (get_element 2 expression)) state))

      ; as above but for right side
      ((eq? (get_element 2 expression) 'true) (M_boolean (list (get_element 0 expression) (get_element 1 expression) #t) state))
      ((eq? (get_element 2 expression) 'false) (M_boolean (list (get_element 0 expression) (get_element 1 expression) #f) state))
      ((and (list? (get_element 2 expression)) (boolean_operator? (car (get_element 2 expression))))
       (M_boolean (list (get_element 0 expression) (get_element 1 expression) (M_boolean (get_element 2 expression) state)) state))
      ((list? (get_element 2 expression))
       (M_boolean (list (get_element 0 expression) (get_element 1 expression) (cadr (M_value (get_element 2 expression) state))) state))

      
      ((eq? (car expression) '&&)
       (M_boolean_tf_to_hashtags (and (M_boolean_truth_finder (get_element 1 expression) state) (M_boolean_truth_finder (get_element 2 expression) state))))
      ((eq? (car expression) '||)
       (M_boolean_tf_to_hashtags (or (M_boolean_truth_finder (get_element 1 expression) state) (M_boolean_truth_finder (get_element 2 expression) state))))
      
      ; if the left or right element is not a number, look up the value of the variable. 
      ((not(number? (get_element 1 expression)))
       (M_boolean (list (get_element 0 expression) (M_state_lookup (get_element 1 expression) state) (get_element 2 expression)) state));added on plane
      ((not(number? (get_element 2 expression)))
       (M_boolean (list (get_element 0 expression) (get_element 1 expression) (M_state_lookup (get_element 2 expression) state) ) state));added on plane


      ((eq? (car expression) '==) (M_boolean_equal (get_element 1 expression) (get_element 2 expression)))
      ((eq? (car expression) '!=) (not (M_boolean_equal (get_element 1 expression) (get_element 2 expression))))
      ((eq? (car expression) '<) (< (get_element 1 expression) (get_element 2 expression)))
      ((eq? (car expression) '>) (> (get_element 1 expression) (get_element 2 expression)))
      ((eq? (car expression) '<=) (<= (get_element 1 expression) (get_element 2 expression)))
      ((eq? (car expression) '>=) (>= (get_element 1 expression) (get_element 2 expression)))

      
      
    )))

(define M_boolean_truth_finder
  (lambda (var state)
    (cond
      ((null? var) #f)
      ((boolean? var) var)
      ((member? var (get_element 0 state)) (M_boolean_tf_to_hashtags(M_state_lookup var state)))
      ((eq? var 'true) #t)
      ((eq? var 'false) #f)
      (else (error "our version of variable not initialized"))
      )))

(define M_boolean_equal
  (lambda (left right)
    (cond
      ((= left right) #t)
      (else #f))))

(define M_boolean_lessthan
  (lambda (left right)
    (cond
      ((< left right) #t)
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
  (lambda var
    (cond
      ((null? (car var)) '())
      ((number? (car var)) (car var))
      ((boolean? (car var)) (car var))
      ((or (eq? 'true (car var)) (eq? 'false (car var))) (M_boolean_tf_to_hashtags (car var)))
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
  (lambda (var state)
    (cond
      ((null? var) 'null)
      ((number? var) var)
      ((member? var (get_element 0 state)) (M_state_lookup var state))  
      ((and (list? var) (boolean_operator? (car var))) (M_boolean var state))
      ((list? var) (cadr (M_value var state)))
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
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-5.txt") 1)) (error "Test 2-5 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-6.txt") 115)) (error "Test 3-6 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-7.txt") 'true)) (error "Test 3-7 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-8.txt") 20)) (error "Test 3-8 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-9.txt") 24)) (error "Test 3-9 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-10.txt") 2)) (error "Test 3-10 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-11.txt") 35)) (error "Test 3-11 failed"))
        ((not (eq? (interpret "Unit Tests/fileToParseTest3-13.txt") 90)) (error "Test 3-13 failed"))
        ;((not (eq? (interpret "Unit Tests/fileToParseTest3-14.txt") 69)) (error "Test 3-14 failed"))
        ;((not (eq? (interpret "Unit Tests/fileToParseTest3-15.txt") 87)) (error "Test 3-15 failed"))
        ;((not (eq? (interpret "Unit Tests/fileToParseTest3-16.txt") 64)) (error "Test 3-16 failed"))
        ;((not (eq? (interpret "Unit Tests/fileToParseTest3-17.txt") 2000400)) (error "Test 2-17 failed"))
        ;((not (eq? (interpret "Unit Tests/fileToParseTest3-18.txt") 101)) (error "Test 2-18 failed"))
        
        (display "all tests passed")
        )))
;(tests3)





;(interpret "Unit Tests/fileToParseTest3-14test.txt")
(interpret "Unit Tests/fileToParseTest3-4.txt")
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

