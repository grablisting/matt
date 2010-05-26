; Matt Forbes
; CSCI 211
; Assignment 4 - Set operations

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; remove
; takes an element and a set and returns the set without that element
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define remove
  (lambda (elem set)
    (cond((null? set) set)
         ((and (list? elem) (list? (car set)))
          (if (set-equal? elem (car set))
              (remove elem (cdr set))
              (cons (car set) (remove elem (cdr set)))))
         ((and (not (list? elem)) (not (list? (car set))))
          (if (eq? elem (car set))
              (remove elem (cdr set))
              (cons (car set) (remove elem (cdr set)))))
         (else (cons (car set) (remove elem (cdr set)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; memb?
; takes an element and a set and returns true if the element is in the set
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define memb?
  (lambda (elem set)
    (cond ((null? set) #f)
          ((and (list? elem) (list? (car set)))
           (or (set-equal? elem (car set)) (memb? elem (cdr set))))
          ((and (not (list? elem)) (not (list? (car set))))
           (or (eq? elem (car set)) (memb? elem (cdr set))))
          (else (memb? elem (cdr set))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; set-equal?
; takes two sets and returns true if they contain exactly the same elements
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define set-equal?
  (lambda (set1 set2)
    (cond((null? set1) (null? set2))
         ((null? set2) (null? set1))
         (else
          (and (memb? (car set1) set2)
               (memb? (car set2) set1)
               (set-equal? (remove (car set2) (cdr set1)) (remove (car set1) (cdr set2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; subset?
; takes two sets and returns true if all of the elements in the first
; set are elements in the second set
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define subset?
  (lambda (set1 set2)
    (if (null? set1)
        #t
        (and (memb? (car set1) set2)
             (subset? (cdr set1) set2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; subset - (assignment has the function with no '?' so here's a copy)
; takes two sets and returns true if all of the elements in the first
; set are elements in the second set
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define subset
  (lambda (set1 set2)
    (if (null? set1)
        #t
        (and (memb? (car set1) set2)
             (subset? (cdr set1) set2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; union
; takes two sets and returns a new set with all of the elements that are
; in either the first set, the second set, or both
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define union
  (lambda (set1 set2)
    (cond((null? set1) set2)
         ((null? set2) set1)
         (else
          (let ((elem (car set1))
                (rest (cdr set1)))
            (if (memb? elem set2)
                (union rest set2)
                (cons elem (union rest set2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; intersection
; takes two sets and returns a new set with only the elements that are
; in both sets
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define intersection
  (lambda (set1 set2)
    (if (or (null? set1) (null? set2))
         '()
         (let ((elem (car set1))
               (rest (cdr set1)))
           (if (memb? elem set2)
               (cons elem (intersection rest set2))
               (intersection rest set2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; difference
; takes two sets and returns a new set that contains all of the elements
; that are in the first set but not the second
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define difference
  (lambda (set1 set2)
    (if (or (null? set1) (null? set2))
        set1
        (let ((elem (car set1))
              (rest (cdr set1)))
          (if (memb? elem set2)
              (difference rest set2)
              (cons elem (difference rest set2)))))))
