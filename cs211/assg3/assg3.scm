;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; div - Divides two given numbers and returns a list with the quotient
;       as the first element, and the remainder as the second. Uses a
;       helper function div_h to recursively subtract the divisor
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define div
  (lambda (x y)
    (div_h x y 0)))

(define div_h
  (lambda (x y z)
    (cond ( (or (< x y) (zero? x)) (cons z (cons x '())) )
          ( else (div_h (- x y) y (+ z 1) ) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; mygcd - Given two integers, will return the greatest common divisor
;         of the input. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mygcd
  (lambda (a b)
    (cond ( (zero? b) a )
          ( (> b a) (mygcd b a))
          ( else (mygcd b (car (cdr (div a b))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; dtb - converts the input integer into binary format. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dtb
  (lambda (decimal)
    (cond ( (zero? decimal) 0)
          ( else (+ (* 10 (dtb (car (div decimal 2)))) (cadr (div decimal 2)) )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; NumberOfNodes - takes a list in the form (leftTree val rightTree)
;                 and returns the number of nodes in the tree.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define NumberOfNodes
  (lambda (tree)
    (cond ( (null? tree) 0)
          ( else (+ 1 (NumberOfNodes (car tree)) (NumberOfNodes (caddr tree)))))))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; NumberOfLeaves - takes a list in the form (leftTree val rightTree)
;                  and returns the number of leaves in the tree.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define NumberOfLeaves
  (lambda (tree)
    (cond ((null? tree) 0) 
          ( (and (null? (car tree)) (null? (caddr tree))) 1)
          ( else (+ (NumberOfLeaves (car tree)) (NumberOfLeaves (caddr tree)))))))

; tree - example tree for testing
(define tree '(((() 11 ()) 8 (() 6 ())) 5 (() 4 (() 2 ((() 3 ()) 22 ((() 4 ()) 3 ()))))))
