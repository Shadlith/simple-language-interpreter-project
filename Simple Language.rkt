#lang racket

; Group 8 - Suvaion Das, Remy Niman, Faraz Mamaghani
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
    (print parsed);just to see what it outputs
    (call/cc (lambda (break)
               (evaluate parsed (list(list(box 'return))) (list(list(box 'null))) break)))
    ))



; if what we have is a list of lists
;ex: ((1 2 3) (1a 2a 3a) (1b 2b 3b))
; if (x > y)
; cadar = 2
; cddar = (3)
; 

;((var x) (= x 10) (var y (+ (* 3 x) 5)) (while (!= (% y x) 3)
; (= y (+ y 1))) (if (> x y) (return x) (if (> (* x x) y) (return (* x x)) (if (> (* x (+ x x)) y) (return (* x (+ x x))) (return (- y 1))))))
(define evaluate
  (lambda (lis declared_list value_list break)
    (newline) (display "declared list at top of evaluate") (display declared_list)
    (newline) (display "value list at top of evaluate") (display value_list)
    (cond
      ((null? lis) (list declared_list value_list))
      ((null? (car lis)) (list declared_list value_list))
      ;if the first word is "var" and this sublist just has the declaration in it (ex: var x)
      ((and (eq? 'var (caar lis)) (eq? '() (cddar lis)))
       (evaluate (cdr lis) (M_state_add_to_declared_list declared_list (cadar lis)) (M_state_add_to_value_list value_list "error") break))
      ;if the first word is "var" and the right is a number (ex: var x = 10)
      ((and (eq? 'var (caar lis)) (number? (get_element 2 (car lis))))
       (evaluate (cdr lis) (M_state_add_to_declared_list declared_list (cadar lis)) (M_state_add_to_value_list value_list (get_element 2 (car lis))) break))
      ; if the first word is var and the 3rd element is a variable. (ex: var x = y (and y is declared)) 
      ((and (eq? 'var (caar lis)) (member? (get_element 2 (car lis)) declared_list))
       (evaluate (cdr lis) (M_state_add_to_declared_list declared_list (cadar lis)) (M_state_add_to_value_list value_list (M_state_lookup(get_element 2 (car lis)) declared_list value_list)) break))
      ; if the first word is var and the 3rd element is a variable and its not in the member list (didn't finish this line...)
      ;((and (eq? 'var (caar lis)) (member? (get_element 2 (car lis)) declared_list))
      ;if the first word is "var" and this sublist is a boolean. ex: var x = a && b
      ((and (eq? 'var (caar lis)) (boolean_operator? (car (caddar lis))))
       (evaluate (cdr lis) (M_state_add_to_declared_list declared_list (cadar lis)) (M_state_add_to_value_list value_list (M_boolean_tf_to_truefalse (M_boolean (caddar lis) declared_list value_list))) break))
      ;if the first word is "var" and there is more in this sublist than just the declaration aka our first word is var and it's not of the others ex: var x = 5+7
      ((eq? 'var (caar lis))
       (evaluate (cdr lis) (M_state_add_to_declared_list declared_list (cadar lis)) (M_state_add_to_value_list value_list (M_value (caddar lis) declared_list value_list)) break))

      ; if it's an assignment statement and it's in the declared list, assuming the second value is a list and a boolean ex: x = a && b
      ((and (and (and (eq? '= (caar lis)) (member? (cadar lis) declared_list)) (list? (caddar lis))) (boolean_operator? (car (caddar lis))))
       (evaluate (cdr lis) declared_list (M_state_modify_value_list declared_list value_list (cadar lis) (M_boolean_tf_to_truefalse (M_boolean (caddar lis) declared_list value_list))) break))
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a list and not a boolean ex: x = 5+7
      ((and (and (eq? '= (caar lis)) (member? (cadar lis) declared_list)) (list? (caddar lis)))
       (evaluate (cdr lis) declared_list (M_state_modify_value_list declared_list value_list (cadar lis) (M_value (caddar lis) declared_list value_list)) break))
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a number ex: x = 5
      ((and (and (eq? '= (caar lis)) (member? (cadar lis) declared_list)) (number? (caddar lis)))
       (evaluate (cdr lis) declared_list (M_state_modify_value_list declared_list value_list (cadar lis) (caddar lis)) break))
      ; if it's an assignment statement and it's in the declared list, assuming the second value is a variable ex: x = y
      ((and (and (eq? '= (caar lis)) (member? (cadar lis) declared_list)) (member? (caddar lis) declared_list))
       (evaluate (cdr lis) declared_list (M_state_modify_value_list declared_list value_list (cadar lis) (M_state_lookup (caddar lis) declared_list value_list)) break))
      ; if it's an assignment statement and gets to this line, it's not in the declared list and should fail. 
      ((eq? '= (caar lis)) (error "our version of variable not initialized"))
      
      ; ?if it starts with an "if" and the condition is true only we're going to move on to the body. It will only get to the else if if the conditino is false. ex:(if (> x y) (return x))
      ;((and (eq? 'if (caar lis)) (M_boolean (cadar lis) declared_list value_list))
       ;(evaluate (cdr lis) (car (evaluate (list (get_element 2 (car lis))) declared_list value_list))))
      
      ; if the list starts with "if" and the condition next to it is true...then evalute it (we think this one is right) 
      ((and (eq? 'if (caar lis)) (M_boolean (cadar lis) declared_list value_list)) (newline)
       (evaluate (cdr lis) (car (evaluate (list (get_element 2 (car lis))) declared_list value_list break)) (cadr (evaluate (list (get_element 2 (car lis))) declared_list value_list break)) break))
      ; if the condition is not true, the list has 4 elements and it starts with an "if"
      ((and (eq? (length (car lis)) 4) (eq? 'if (caar lis)))
       (evaluate (cdr lis) (car (evaluate (list (get_element 3 (car lis))) declared_list value_list break)) (cadr (evaluate (list (get_element 3 (car lis))) declared_list value_list break)) break))

      ; if the list starts with "if", but the condition is not true and there is a 4th element. 
     ; ((and (eq? 'if (caar lis)) (not(eq? '() (get_element 3 (car lis)))))
       ;(evaluate (cdr lis) (evaluate (list (list (get_element 3 (car lis)))) declared_list value_list) (evaluate (list (get_element 3 (car lis))) declared_list value_list)))

      ; if the list starts with "if", but the condition is not true
      ((eq? 'if (caar lis)) (evaluate (cdr lis) declared_list value_list break))

      ; when it's a "while" and the condition is true
      ((and (eq? 'while (caar lis)) (M_boolean (cadar lis) declared_list value_list))
       ;(evaluate lis (car (evaluate (list (get_element 2 (car lis))) declared_list value_list break)) (cadr (evaluate (list (get_element 2 (car lis))) declared_list value_list break)) break))
       ;(evaluate lis (M_state_add_row_to_declared_list declared_list) (cadr (evaluate (list (get_element 2 (car lis))) declared_list value_list break)) break))
       (evaluate lis declared_list (cadr (evaluate (list (get_element 2 (car lis))) declared_list value_list break)) break))

      ;when it's a "while" and by default, the boolean is false. 
      ((eq? 'while (caar lis)) (evaluate (cdr lis) declared_list value_list break))

      ; when it's return and a boolean, this gets it to be "true" or "false"
      ((and(eq? 'return (caar lis)) (boolean? (M_state_lookup 'return declared_list (M_state_modify_value_list declared_list value_list 'return (M_state_return_helper (cadar lis) declared_list value_list))))
       (M_boolean_truefalse_converter(M_state_lookup 'return declared_list (M_state_modify_value_list declared_list value_list 'return (M_state_return_helper (cadar lis) declared_list value_list))))))

      ((and (eq? 'return (caar lis)) (number? (cadar lis)))
        (break (cadar lis)))
        ;(M_state_modify_value_list declared_list value_list 'return (M_state_return_helper (cadar lis) declared_list value_list)))

      ; if it's a return and an expression in after the return (we think this is unnecessary for Part 1 and is taken care of in the next chunk)
      ;((and (eq? 'return (caar lis)) (operator? (caadar lis))) (newline) (display "return expression: ") (break (M_value (cadar lis) declared_list value_list))) 
      
      ((eq? 'return (caar lis))
       (M_state_lookup 'return declared_list (M_state_modify_value_list declared_list value_list 'return (M_state_return_helper (cadar lis) declared_list value_list))))

      ; if it starts with "begin"....we think we do this. THIS IS NOT DONE.....need to figure out boxes. Need to have things added to declared list on the top row.
      ; 3-14: We need to do the "remove top layer" for value list once the "begin" sublist has ended. Worried about nested begins. 
      ; Actually think we're never going to remove the top layer from the declared list? Maybe. 
      ((eq? 'begin (caar lis)) (evaluate (cdr lis) declared_list
                                         (M_state_remove_top_layer_value_list (cadr (evaluate (cdar lis) (M_state_add_row_to_declared_list declared_list) (M_state_add_row_to_value_list value_list) break))) break))
      
      )))

;'((var x 10) (begin (var y 2) (var z (* x y)) (= x z)) (return x))

(define M_state_add_row_to_declared_list
  (lambda declared_list (newline) (display "updated declared list: ") (display (cons '() (car declared_list))) (newline)
    (cons '() (car declared_list))
    ))

(define M_state_add_row_to_value_list
  (lambda value_list (newline) (display "updated value list: ") (display (cons '() (car value_list))) (newline)
    (cons '() (car value_list))
    ))

(define M_state_add_to_declared_list
  (lambda (declared_list var)
    (cons (cons (box var) (car declared_list)) (cdr declared_list))
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
      ;((not(or (or (number? val) (eq? 'true val)) (eq? 'false val))) (error "our version of variable not initialized"))
      ((list? val) (error "val is a list for some reason"))
      (else (cons (cons (box val) (car value_list)) (cdr value_list)))
       )))

;if the variable is already in the declared-list, then this changes the value in the value list.
(define M_state_modify_value_list
  (lambda (declared_list value_list var newval)
    (cond
      ((null? declared_list) (error "our version of variable not initialized"))
      ((and (list? (car declared_list)) (member? var (car declared_list))) (cons (M_state_modify_value_list (car declared_list) (car value_list) var newval) (cdr value_list)))
      ((list? (car declared_list)) (cons (car value_list) (M_state_modify_value_list (cdr declared_list) (cdr value_list) var newval)))
      ; 3-12: This eq line is where we have an issue.....both don't work
      ((eq? var (unbox (car declared_list))) (cons (box newval) (cdr value_list)))
      ; the set-box! works.....except for the last test of Part 1.....so, we're not going to do it (for now)
      ;((eq? var (unbox (car declared_list))) (begin (set-box! (car value_list) newval) value_list))
      (else (cons (car value_list) (M_state_modify_value_list (cdr declared_list) (cdr value_list) var newval)))
    )))

(define M_state_lookup
  (lambda (var declared_list value_list)
    (cond
      ((null? declared_list) (error "variable not found"))
      ;((null? (car declared_list)) (M_state_lookup var (cdr declared_list) (cdr value_list)))
      ((and (and (list? (car declared_list)) (member? var (car declared_list))) (not(null? (car declared_list)))) (M_state_lookup var (car declared_list) (car value_list)))
      ((list? (car declared_list)) (M_state_lookup var (cdr declared_list) (cdr value_list)))
      ((eq? var (unbox (car declared_list))) (variable_type?(unbox (car value_list))))
      (else (M_state_lookup var (cdr declared_list) (cdr value_list)))
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


; variable_type tells us if the variable is declared or not. If it's 'true' or 'false' it converts it to a hashtag
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
  (lambda (var declared_list value_list)
    (cond
      ((null? var) 'null)
      ((number? var) var)
      ((member? var declared_list) (M_state_lookup var declared_list value_list))  
      ((and (list? var) (boolean_operator? (car var))) (M_boolean var declared_list value_list))
      ((list? var) (M_value var declared_list value_list))
      (else (error "invalid return"))
       )))

#|
(eq? (interpret "Unit Tests/fileToParseTest2-1.txt") 20)
(eq? (interpret "Unit Tests/fileToParseTest2-2.txt") 164)
(eq? (interpret "Unit Tests/fileToParseTest2-3.txt") 32)
(eq? (interpret "Unit Tests/fileToParseTest2-4.txt") 2)
;(interpret "Unit Tests/fileToParseTest2-5.txt")
(eq? (interpret "Unit Tests/fileToParseTest2-6.txt") 25)
|#
(eq? (interpret "Unit Tests/fileToParseTest2-7.txt") 21)
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
(eq? (interpret "Unit Tests/fileToParseTest1-15.txt") "true")
(eq? (interpret "Unit Tests/fileToParseTest1-16.txt") 100)
(eq? (interpret "Unit Tests/fileToParseTest1-17.txt") "false")
(eq? (interpret "Unit Tests/fileToParseTest1-18.txt") "true")
(eq? (interpret "Unit Tests/fileToParseTest1-19.txt") 128)
(eq? (interpret "Unit Tests/fileToParseTest1-20.txt") 12)
|#