#lang racket

; Group 8 - Suvaion Das, Remy Nima, Faraz Mamaghani
; Simple Language Interpreter


(require "simpleParser.rkt")

(define parse
  (lambda (filename)
    (parser filename)
    ))

;code to run on: (interpret "fileToParse.txt")
(define interpret
  (lambda (filename)
    (define parsed (parse filename))
    ;(print parsed);just to see what it outputs
    (evaluate parsed '(return) '(null))
    ))

(define evaluate
  (lambda (lis declared_list value_list)
    (cond
      ((null? lis) (list declared_list value_list))
      ((null? (car lis)) (list declared_list value_list))
      ;if the first word is "var" and this sublist just has the declaration in it
      ((and (eq? 'var (caar lis)) (eq? '() (cddar lis)))
       (evaluate (cdr lis) (M_state_add_to_declared_list declared_list (cadar lis)) (M_state_add_to_value_list value_list "error")))
      ;if the first word is "var" and the right is a number
      ((and (eq? 'var (caar lis)) (number? (get_element 2 (car lis))))
       (evaluate (cdr lis) (M_state_add_to_declared_list declared_list (cadar lis)) (M_state_add_to_value_list value_list (get_element 2 (car lis)))))
      ; if the first word is var and the 3rd element is a variable. 
      ((and (eq? 'var (caar lis)) (member? (get_element 2 (car lis)) declared_list))
       (evaluate (cdr lis) (M_state_add_to_declared_list declared_list (cadar lis)) (M_state_add_to_value_list value_list (M_state_lookup(get_element 2 (car lis)) declared_list value_list))))
      ; if the first word is var and the 3rd element is a variable and its not in the member list (didn't finish this line...)
      ;((and (eq? 'var (caar lis)) (member? (get_element 2 (car lis)) declared_list))
      ;if the first word is "var" and this sublist is a boolean
      ((and (eq? 'var (caar lis)) (boolean_operator? (car (caddar lis))))
       (evaluate (cdr lis) (M_state_add_to_declared_list declared_list (cadar lis)) (M_state_add_to_value_list value_list (M_boolean_tf_to_truefalse (M_boolean (caddar lis) declared_list value_list)))))
      ;if the first word is "var" and there is more in this sublist than just the declaration
      ((eq? 'var (caar lis))
       (evaluate (cdr lis) (M_state_add_to_declared_list declared_list (cadar lis)) (M_state_add_to_value_list value_list (M_value (caddar lis) declared_list value_list))))
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a list and a boolean
      ((and (and (and (eq? '= (caar lis)) (member? (cadar lis) declared_list)) (list? (caddar lis))) (boolean_operator? (car (caddar lis))))
       (evaluate (cdr lis) declared_list (M_state_modify_value_list declared_list value_list (cadar lis) (M_boolean_tf_to_truefalse (M_boolean (caddar lis) declared_list value_list)))))
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a list and not a boolean
      ((and (and (eq? '= (caar lis)) (member? (cadar lis) declared_list)) (list? (caddar lis)))
       (evaluate (cdr lis) declared_list (M_state_modify_value_list declared_list value_list (cadar lis) (M_value (caddar lis) declared_list value_list))))
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a number
      ((and (and (eq? '= (caar lis)) (member? (cadar lis) declared_list)) (number? (caddar lis)))
       (evaluate (cdr lis) declared_list (M_state_modify_value_list declared_list value_list (cadar lis) (caddar lis))))
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a variable
      ((and (and (eq? '= (caar lis)) (member? (cadar lis) declared_list)) (member? (caddar lis) declared_list))
       (evaluate (cdr lis) declared_list (M_state_modify_value_list declared_list value_list (cadar lis) (M_state_lookup (caddar lis) declared_list value_list))))
      ; if it's an assignment statement and gets to this line, it's not in the declared list and should fail. 
      ((eq? '= (caar lis)) (error "our version of variable not initialized")) 
      ((and (and (eq? 'if (caar lis)) (M_boolean (cadar lis) declared_list value_list) (eq? (length (caddar lis)) 1)))
       (evaluate (cdr lis) (car (evaluate (list (get_element 2 (car lis))) declared_list value_list))))
      ; if the list starts with "if" and the condition next to it is true...then....not sure if the then part is correct. 
      ((and (eq? 'if (caar lis)) (M_boolean (cadar lis) declared_list value_list))
       (evaluate (cdr lis) (car (evaluate (list (get_element 2 (car lis))) declared_list value_list)) (cadr (evaluate (list (get_element 2 (car lis))) declared_list value_list))))
      ; if the condition is not true, the list has 4 elements and it starts with an "if"
      ((and (eq? (length (car lis)) 4) (eq? 'if (caar lis)))
       (evaluate (cdr lis) (car (evaluate (list (get_element 3 (car lis))) declared_list value_list)) (cadr (evaluate (list (get_element 3 (car lis))) declared_list value_list))))

      ; if the list starts with "if", but the condition is not true and there is a 4th element. 
      ((and (eq? 'if (caar lis)) (not(eq? '() (get_element 3 (car lis)))))
       (evaluate (cdr lis) (evaluate (list (list (get_element 3 (car lis)))) declared_list value_list) (evaluate (list (get_element 3 (car lis))) declared_list value_list)))

      ; if the list starts with "if", but the condition is not true
      ((eq? 'if (caar lis)) (evaluate (cdr lis) declared_list value_list))

      ((and (eq? 'while (caar lis)) (M_boolean (cadar lis) declared_list value_list))
       (evaluate lis (car (evaluate (list (get_element 2 (car lis))) declared_list value_list)) (cadr (evaluate (list (get_element 2 (car lis))) declared_list value_list))))
      ((eq? 'while (caar lis)) (evaluate (cdr lis) declared_list value_list))

      ((and(eq? 'return (caar lis)) (boolean? (M_state_lookup 'return declared_list (M_state_modify_value_list declared_list value_list 'return (M_state_return_helper (cadar lis) declared_list value_list)))))
       (M_boolean_truefalse_converter(M_state_lookup 'return declared_list (M_state_modify_value_list declared_list value_list 'return (M_state_return_helper (cadar lis) declared_list value_list)))))

      ((and (eq? 'return (caar lis)) (number? (cadar lis))) (cadar lis))
      ((eq? 'return (caar lis)) (M_state_lookup 'return declared_list (M_state_modify_value_list declared_list value_list 'return (M_state_return_helper (cadar lis) declared_list value_list))))

      )))

(define M_state_add_to_declared_list
  (lambda (declared_list var)
    (cons var declared_list)
    ))

(define M_state_add_to_value_list
  (lambda (value_list val)
    (cond
      ;((not(or (or (number? val) (eq? 'true val)) (eq? 'false val))) (error "our version of variable not initialized"))
      ((list? val) (error "our version of variable not initialized"))
      (else (cons val value_list))
       )))

;if the variable is already in the declared-list, then this changes the value in the value list.
(define M_state_modify_value_list
  (lambda (declared_list value_list var newval)
    (cond
      ((null? declared_list) (error "our version of variable not initialized"))
      ((eq? var (car declared_list)) (cons newval (cdr value_list)))
      (else (cons (car value_list) (M_state_modify_value_list (cdr declared_list) (cdr value_list) var newval)))
    )))

(define M_value
  (lambda (expression declared_list value_list)
    (cond
      ((null? expression) '())
      ((number? expression) expression)
      ((and (and (eq? (length expression) 2) (eq? (car expression) '-))) 
       (M_value_mult -1 (M_value (get_element 1 expression) declared_list value_list)))
       
      ((and (and (eq? (car expression) '+) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (M_value_add (get_element 1 expression) (get_element 2 expression)))
      
      ((and (and (eq? (car expression) '-) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (M_value_sub (get_element 1 expression) (get_element 2 expression)))

     
      
      ((and (and (eq? (car expression) '*) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (M_value_mult (get_element 1 expression) (get_element 2 expression)))
      
      ((and (and (eq? (car expression) '/) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (M_value_div (get_element 1 expression) (get_element 2 expression)))
      
      ((and (and (eq? (car expression) '%) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (M_value_mod (get_element 1 expression) (get_element 2 expression)))

      ((list? (get_element 1 expression))
       (M_value (list (get_element 0 expression)
                             (M_value (get_element 1 expression) declared_list value_list) (get_element 2 expression)) declared_list value_list))

      ((list? (get_element 2 expression))
       (M_value (list (get_element 0 expression)
                              (get_element 1 expression)  (M_value (get_element 2 expression) declared_list value_list)) declared_list value_list))

      ((member? (get_element 1 expression) declared_list)
       (M_value (list (get_element 0 expression)
                             (M_state_lookup (get_element 1 expression) declared_list value_list) (get_element 2 expression)) declared_list value_list))

      ((member? (get_element 2 expression) declared_list)
       (M_value (list (get_element 0 expression)
                              (get_element 1 expression)  (M_state_lookup (get_element 2 expression) declared_list value_list)) declared_list value_list))
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
  (lambda (expression declared_list value_list)
    (cond
      ((null? expression) false)
      ((and (and (list? (get_element 1 expression)) (eq? (car expression) '!)) (boolean_operator? (car (get_element 1 expression)))) (M_boolean (list (get_element 0 expression) (M_boolean (get_element 1 expression) declared_list value_list)) declared_list value_list))
      ((eq? (car expression) '!) (M_boolean_tf_to_hashtags (not (M_boolean_truth_finder (get_element 1 expression) declared_list value_list))))
      
      ((boolean? expression) expression)
     
      ;Checks if left side is a boolean equation or a integer and calcs it
      ((eq? (get_element 1 expression) 'true) (M_boolean (list (get_element 0 expression) #t (get_element 2 expression)) declared_list value_list))
      ((eq? (get_element 1 expression) 'false) (M_boolean (list (get_element 0 expression) #f (get_element 2 expression)) declared_list value_list))
      ((and (list? (get_element 1 expression)) (boolean_operator? (car (get_element 1 expression)))) (M_boolean (list (get_element 0 expression) (M_boolean (get_element 1 expression) declared_list value_list) (get_element 2 expression)) declared_list value_list))
      ((list? (get_element 1 expression)) (M_boolean (list (get_element 0 expression) (M_value (get_element 1 expression) declared_list value_list) (get_element 2 expression)) declared_list value_list))
      ; as above but for right side
      ((eq? (get_element 2 expression) 'true) (M_boolean (list (get_element 0 expression) (get_element 1 expression) #t) declared_list value_list))
      ((eq? (get_element 2 expression) 'false) (M_boolean (list (get_element 0 expression) (get_element 1 expression) #f) declared_list value_list))
      ((and (list? (get_element 2 expression)) (boolean_operator? (car (get_element 2 expression)))) (M_boolean (list (get_element 0 expression) (get_element 1 expression) (M_boolean (get_element 2 expression) declared_list value_list)) declared_list value_list))
      ((list? (get_element 2 expression)) (M_boolean (list (get_element 0 expression) (get_element 1 expression) (M_value (get_element 2 expression) declared_list value_list)) declared_list value_list))

      
      ((eq? (car expression) '&&) (M_boolean_tf_to_hashtags (and (M_boolean_truth_finder (get_element 1 expression) declared_list value_list) (M_boolean_truth_finder (get_element 2 expression) declared_list value_list))))
      ((eq? (car expression) '||) (M_boolean_tf_to_hashtags (or (M_boolean_truth_finder (get_element 1 expression) declared_list value_list) (M_boolean_truth_finder (get_element 2 expression) declared_list value_list))))
      
      ; if the left or right element is not a number, look up the value of the variable. 
      ((not(number? (get_element 1 expression))) (M_boolean (list (get_element 0 expression) (M_state_lookup (get_element 1 expression) declared_list value_list) (get_element 2 expression)) declared_list value_list));added on plane
      ((not(number? (get_element 2 expression))) (M_boolean (list (get_element 0 expression) (get_element 1 expression) (M_state_lookup (get_element 2 expression) declared_list value_list) ) declared_list value_list));added on plane


      ((eq? (car expression) '==) (M_boolean_equal (get_element 1 expression) (get_element 2 expression)))
      ((eq? (car expression) '!=) (not (M_boolean_equal (get_element 1 expression) (get_element 2 expression))))
      ((eq? (car expression) '<) (< (get_element 1 expression) (get_element 2 expression)))
      ((eq? (car expression) '>) (> (get_element 1 expression) (get_element 2 expression)))
      ((eq? (car expression) '<=) (<= (get_element 1 expression) (get_element 2 expression)))
      ((eq? (car expression) '>=) (>= (get_element 1 expression) (get_element 2 expression)))

      
      
    )))

(define M_boolean_truth_finder
  (lambda (var declared_list value_list)
    (cond
      ((null? var) #f)
      ((boolean? var) var)
      ((member? var declared_list) (M_boolean_tf_to_hashtags(M_state_lookup var declared_list value_list)))
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

(define M_state_lookup
  (lambda (var declared_list value_list)
    (cond
      ((null? declared_list) '())
      ((eq? var (car declared_list)) (variable_type?(car value_list)))
      (else (M_state_lookup var (cdr declared_list) (cdr value_list)))
      )))

(define variable_type?
  (lambda var
    (cond
      ((null? (car var)) '())
      ((number? (car var)) (car var))
      ((boolean? (car var)) (car var))
      ((or (eq? 'true (car var)) (eq? 'false (car var))) (M_boolean_tf_to_hashtags (car var)))
      (else (error "our version of variable not initialized"))
      )))

(define member?
  (lambda (x lis)
    (cond
      ((null? lis) #f)
      ((eq? x (car lis)) #t)
      (else (member? x (cdr lis))))))

(define boolean_operator?
  (lambda (var)
    (cond
      ((member? var (list '== '!= '< '> '<= '>= '&& '|| '!)) #t)
      (else #f)
      )))

(define get_element
  (lambda (index lis);first item = 0
    (cond
      ((null? lis) '())
      ((eq? index 0) (car lis))
      (else (get_element (- index 1) (cdr lis)))
      )))

(define M_state_return_helper
  (lambda (var declared_list value_list)
    (cond
      ((null? var) 'null)
      ((number? var) var)
      ((member? var declared_list) (M_state_lookup var declared_list value_list))  
      ((boolean_operator? (car var)) (M_boolean var declared_list value_list))
      (else (M_value var declared_list value_list))
       )))