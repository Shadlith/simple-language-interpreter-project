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

;Remy: This is good for the first 3 cond lines. Next, we need to figure out assignment. It will likely get called where we have "stuff" right now.  
(define evaluate
  (lambda (lis declared_list value_list)
    (cond
      ((null? (car lis)) ('()))
      ((and (eq? 'var (caar lis)) (eq? '() (cddar lis))) (evaluate (cdr lis) (M_state_add_to_declared_list declared_list (cadar lis)) (M_state_add_to_value_list value_list "error")))
      ((eq? 'var (caar lis)) (evaluate (cdr lis) (M_state_add_to_declared_list declared_list (cadar lis)) (M_state_add_to_value_list value_list "stuff")))
      ;((eq? '= (caar lis)) (M_state_add_to_value_list value_list (cddar lis) declared_list (cadar lis))); Remy: this needs evalute run on the cdr at the end of this line
      ; Remy: lots of other stuff belongs here
      )))

(define M_state_add_to_declared_list
  (lambda (declared_list var)
    (cons var declared_list)
    ))

(define M_state_add_to_value_list
  (lambda (value_list val)
      (cons val value_list)
       ))

