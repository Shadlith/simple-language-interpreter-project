#lang racket

; Remy: To start, go and do the "abstractoin practice" in Module 3. You can also just look at the posted comments of answers for the last "abstraction practice".
; Remy: Next, read my notes. They explain some of my decisions. https://docs.google.com/document/d/1FqZTvddmMBunkmgT6yZkYWjuNLRvjeoO4_Zh7lqix0w/edit?usp=sharing
; Remy: Here is what the parser outputs for the example problem given in the assignment:
;'((var x)
; (= x 10)
; (var y (+ (* 3 x) 5))
; (while (!= (% y x) 3) (= y (+ y 1)))
; (if (> x y) (return x) (if (> (* x x) y) (return (* x x)) (if (> (* x (+ x x)) y) (return (* x (+ x x))) (return (- y 1))))))


(require "simpleParser.rkt")

(define parse
  (lambda (filename)
    (parser filename)
    ))

;code to run on: (interpret "fileToParse.txt")
(define interpret
  (lambda (filename)
    (define parsed (parse filename))
    (print parsed);just to see what it outputs
    (evaluate parsed '() '())
    ))

(define evaluate
  (lambda (lis declared_list value_list)
    (cond
      ((null? lis) (display declared_list) (display value_list))
      ((null? (car lis)) (display declared_list) (display value_list))
      ;if the first word is "var" and this sublist just has the declaration in it
      ((and (eq? 'var (caar lis)) (eq? '() (cddar lis))) (evaluate (cdr lis) (M_state_add_to_declared_list declared_list (cadar lis)) (M_state_add_to_value_list value_list "error")))
      ;if the first word is "var" and this sublist is a boolean
      ((and (eq? 'var (caar lis)) (boolean_operator? (car (caddar lis)))) (evaluate (cdr lis) (M_state_add_to_declared_list declared_list (cadar lis)) (M_state_add_to_value_list value_list (truefalse_converter (M_boolean (caddar lis) declared_list value_list)))))
      ;if the first word is "var" and there is more in this sublist than just the declaration
      ((eq? 'var (caar lis)) (evaluate (cdr lis) (M_state_add_to_declared_list declared_list (cadar lis)) (M_state_add_to_value_list value_list (M_integer (caddar lis) declared_list value_list))))
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a list and a boolean
      ((and (and (and (eq? '= (caar lis)) (member? (cadar lis) declared_list)) (list? (caddar lis))) (boolean_operator? (car (caddar lis)))) (evaluate (cdr lis) declared_list (M_modify_value_list declared_list value_list (cadar lis) (truefalse_converter (M_boolean (caddar lis) declared_list value_list)))))
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a list and not a boolean
      ((and (and (eq? '= (caar lis)) (member? (cadar lis) declared_list)) (list? (caddar lis))) (evaluate (cdr lis) declared_list (M_modify_value_list declared_list value_list (cadar lis) (M_integer (caddar lis) declared_list value_list))))
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a number
      ((and (and (eq? '= (caar lis)) (member? (cadar lis) declared_list)) (number? (caddar lis))) (evaluate (cdr lis) declared_list (M_modify_value_list declared_list value_list (cadar lis) (caddar lis))))
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a variable
      ((and (and (eq? '= (caar lis)) (member? (cadar lis) declared_list)) (member? (caddar lis) declared_list)) (evaluate (cdr lis) declared_list (M_modify_value_list declared_list value_list (cadar lis) (M_state_lookup (caddar lis) declared_list value_list))))
      ;finish
  ;                                                                                  ^^^^
      ; Remy: (arrow from above) This line is really close, but we need a "M_state_remove_from_declared_list" function to run before this/the result of that be the input to the M_state_add_to_declared_list function  
      
      ; if the list starts with "if" and the condition next to it is true...then....not sure if the then part is correct. 
      ((and (eq? 'if (caar lis)) (M_boolean (cadar lis) declared_list value_list)) (evaluate (evaluate (caddr lis) declared_list value_list) declared_list value_list)) ; added on plane 

      )))

(define M_state_add_to_declared_list
  (lambda (declared_list var)
    (cons var declared_list)
    ))

(define M_state_add_to_value_list
  (lambda (value_list val)
      (cons val value_list)
       ))

;if the variable is already in the declared-list, then this changes the value in the value list.
(define M_modify_value_list
  (lambda (declared_list value_list var newval)
    (cond
      ((null? declared_list) value_list)
      ((eq? var (car declared_list)) (cons newval (cdr value_list)))
      (else (cons (car value_list) (M_modify_value_list (cdr declared_list) (cdr value_list) var newval)))
    )))
;(M_integer (+ 3 5))
;(M_integer (-(+ 3 5)10))
;(M_integer (*(+ 3 5)10))
;(M_integer (/(+ 3 5)10))
;(M_integer (/(+ 3.0 5.0)10.0))
;(M_integer (+(/ 6 2)10))
; when this...var z = 6 * (8 + (5 % 3)) / 11 - 9;...is parsed, we get this.... -> (var z (- (/ (* 6 (+ 8 (% 5 3))) 11) 9)). 
;(M_integer (- (/ (* 6 (+ 8 (% 5 3))) 11) 9))....for some reason it is saying that "%" is undefined...
;(M_integer (- (/ (* 6 (+ 8 (/ 6 3))) 10) 9))....answer should be: -3
; when this....var zz = 6 * (8 + (6 / 3)) - 10 /2;...is parsed, we get this.... -> (- (* 6 (+ 8 (/ 6 3))) (/ 10 2))....this is an actual test of precedence....the 10/2 should be evaluated before the minus. Looks like the parser takes care of this for us! 
;(M_integer (- (* 6 (+ 8 (/ 6 3))) (/ 10 2)))...answer should be 55


; ((*(+ 3 5)10)) (var x))
; car: (*(+ 3 5)10))
; cdr: (var x)
; cdar: (+ 3 5)10)
; cadar: (+ 3 5)
; caddar: 10
; caar: * 


;Remy: Almost certain that the parser takes care of precendence issues for us. 
(define M_integer
  (lambda (expression declared_list value_list)
    (cond
      ((null? expression) '())
      ((number? expression) expression)
      ((and (and (eq? (car expression) '+) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (add (get_element 1 expression) (get_element 2 expression)))
      
      ((and (and (eq? (car expression) '-) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (sub (get_element 1 expression) (get_element 2 expression)))
      
      ((and (and (eq? (car expression) '*) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (mult (get_element 1 expression) (get_element 2 expression)))
      
      ((and (and (eq? (car expression) '/) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (div (get_element 1 expression) (get_element 2 expression)))
      
      ((and (and (eq? (car expression) '%) (number? (get_element 1 expression))) (number? (get_element 2 expression)))
       (mod (get_element 1 expression) (get_element 2 expression)))

      ((list? (get_element 1 expression))
       (M_integer (list (get_element 0 expression)
                             (M_integer (get_element 1 expression) declared_list value_list) (get_element 2 expression)) declared_list value_list))

      ((list? (get_element 2 expression))
       (M_integer (list (get_element 0 expression)
                              (get_element 1 expression)  (M_integer (get_element 2 expression) declared_list value_list)) declared_list value_list))

      ((member? (get_element 1 expression) declared_list)
       (M_integer (list (get_element 0 expression)
                             (M_state_lookup (get_element 1 expression) declared_list value_list) (get_element 2 expression)) declared_list value_list))

      ((member? (get_element 2 expression) declared_list)
       (M_integer (list (get_element 0 expression)
                              (get_element 1 expression)  (M_state_lookup (get_element 2 expression) declared_list value_list)) declared_list value_list))
     ; ((list? (car expression)) (M_integer (car expression) declared_list value_list))
      ;(else (
             
      ;((and (list? left) (list? right))(M_integer (list(M_integer left declared_list value_list)) op (M_integer right declared_list value_list))); if both the left and right of the operator are lists
     ; ((list? left) (M_integer (list (M_integer left declared_list value_list)) declared_list value_list)); if both aren't lists and left is a list, but the left is a list...
      ;((list? right) (M_integer (list op left (M_integer (car right) declared_list value_list))));if both aren't lists and right is a list, the left isn't a list, but the right is a list
      ;((not (number? left)) (M_integer (cons (M_state_lookup left declared_list value_list) right) declared_list value_list));see M_state_lookup below. It's not working and causing an infinite loop.   
      ;((not (number? right)) (M_integer (cons left (M_state_lookup right declared_list value_list)) declared_list value_list))
     
      ; TO-DO: Remy: Idk what the assignment means when it says we need to implement the "unary -". Does that mean negate/make neg if pos and pos if neg? 
      ;(else (0))
      )))


(define do_op
  (lambda (op left right)
    (cond
      ((eq? op '+) (add left right))
      ((eq? op '-) (sub left right))
      ((eq? op '*) (mult left right))
      ((eq? op '/) (div left right))
      ((eq? op '%) (mod left right))
      )))

(define add
  (lambda (left right)
    (cond
      ((or (null? left) (null? right)) 0)
      (else (+ left right))
    )))

(define sub
  (lambda (left right)
    (cond
      ((or (null? left) (null? right)) 0)
      (else (- left right))
    )))

(define mult
  (lambda (left right)
    (cond
      ((or (null? left) (null? right)) 0)
      (else (* left right))
    )))

(define div
  (lambda (left right)
    (cond
      ((or (null? left) (null? right)) 0)
      (else (/ left right))
    )))

(define mod
  (lambda (left right)
    (cond
      ((or (null? left) (null? right)) 0)
      (else (mod left right))
    )))


(define M_boolean
  (lambda (expression declared_list value_list)
    (cond
      ((null? expression) false)
      ;Checks if left side is a boolean equation or a integer and calcs it
      ((and (list? (get_element 1 expression)) (boolean_operator? (car (get_element 1 expression)))) (M_boolean (list (get_element 0 expression) (M_boolean (get_element 1 expression) declared_list value_list) (get_element 2 expression)) declared_list value_list))
      ((list? (get_element 1 expression)) (M_boolean (list (get_element 0 expression) (M_integer (get_element 1 expression) declared_list value_list) (get_element 2 expression)) declared_list value_list))
      ; as above but for right side
      ((and (list? (get_element 2 expression)) (boolean_operator? (car (get_element 2 expression)))) (M_boolean (list (get_element 0 expression) (get_element 1 expression)) (M_boolean (get_element 2 expression) declared_list value_list) declared_list value_list))
      ((list? (get_element 2 expression)) (M_boolean (list (get_element 0 expression) (get_element 1 expression) (M_integer (get_element 2 expression) declared_list value_list)) declared_list value_list))
      ; if the left or right element is not a number, look up the value of the variable. 
      ((not(number? (get_element 1 expression))) (M_boolean (list (get_element 0 expression) (M_state_lookup (get_element 1 expression) declared_list value_list) (get_element 2 expression)) declared_list value_list));added on plane
      ((not(number? (get_element 2 expression))) (M_boolean (list (get_element 0 expression) (get_element 1 expression) (M_state_lookup (get_element 2 expression) declared_list value_list) ) declared_list value_list));added on plane


      ((eq? (car expression) '==) (equal (get_element 1 expression) (get_element 2 expression)))
      ((eq? (car expression) '!=) (not (equal (get_element 1 expression) (get_element 2 expression))))
      ((eq? (car expression) '<) (< (get_element 1 expression) (get_element 2 expression)))
      ((eq? (car expression) '>) (> (get_element 1 expression) (get_element 2 expression)))
      ((eq? (car expression) '<=) (<= (get_element 1 expression) (get_element 2 expression)))
      ((eq? (car expression) '>=) (>= (get_element 1 expression) (get_element 2 expression)))
      
    )))

(define equal
  (lambda (left right)
    (cond
      ((= left right) #t)
      (else #f))))

(define lessthan
  (lambda (left right)
    (cond
      ((< left right) #t)
      (else #f))))


    
(define truefalse_converter
  (lambda (var)
    (cond
      ((eq? var 'true) #t)
      ((eq? var #t) 'true)
      ((eq? var 'false) #f)
      ((eq? var #f) 'false)
      )))

(define M_state_lookup
  (lambda (var declared_list value_list)
    (cond
      ((null? declared_list) '())
      ((eq? var (car declared_list)) (car value_list));Remy: this line is currently not working and is causing an infinite loop. maybe the cars/cdrs are wrong, but this should be matching on the first iteration through for "x=10". 
      (else (M_state_lookup var (cdr declared_list) (cdr value_list)))
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