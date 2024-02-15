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

;Remy: This is what I thought we should do at first....make a list that we keep adding to...but I think we should get rid of this
;Remy: b/c he said in the live session we shouldn't do this....it's not good functional programming/shouldn't be necessary. 
(define declared_list '())
(define value_list '())


(define parse
  (lambda (filename)
    (parser filename)
    ))

;code to run on: (interpret "fileToParse.txt")
(define interpret
  (lambda (filename)
    (define parsed (parse filename))
    (print parsed);just to see what it outputs
    (evaluate parsed declared_list value_list)
    ))

;Remy: This may need its inputs adjusted. 
(define evaluate
  (lambda (lis declared_list value_list)
    (cond
      ((null? (car lis)) (print 1)); "print 1" is just dummy stuff to see if it was hitting that line correctly. 
      ((eq? 'var (caar lis)) (M_state_add_to_declared_list declared_list (cdar lis)) (evaluate (cdr lis) declared_list value_list))
      ((eq? '= (caar lis)) (M_state_add_to_value_list value_list (cddar lis) declared_list (cadar lis))); Remy: this needs evalute run on the cdr at the end of this line
      ; Remy: lots of other stuff belongs here
      (evaluate(cdr lis))
      )))

; Remy: I'm trying to do the second option mentioned in the homework where you have 2 lists for bindings....one with variables and one with values. 
; Remy: need to change this just add to the declared list.....should probably use cons instead of append. 
(define M_state_add_to_declared_list
  (lambda (declared_list var)
    (append declared_list var)
    (print declared_list)
    ))

               
; Remy: idk if this needs all of these inputs I have here. Maybe this should be combined with the above? Maybe it should stay separate for abstraction purposes...
; Remy: regardless, adding to the value list needs to be tied to (aka also make changes at the same time) the declared list addition if it's an assignment statement.
; Remy: Also, for assignment statements, we need to remove the value from the list first. 
(define M_state_add_to_value_list
  (lambda (value_list val declared_list var)
    (cond
      ((null? declared_list) (print ("ERROR: variable not declared yet")))
      ((eq? var (car declared_list)) (cons val (car value_list)))
      (else (cons (car declared_list) (M_state_add_to_value_list (cdr value_list) val (cdr declared_list) var)))
       )))

