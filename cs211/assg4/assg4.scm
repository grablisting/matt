(define memb?
  (lambda (elem set)
    (cond ((null? set) #f)
          ((list? elem) 
           (if (list? (car set)) 
               (or (set-equal? elem (car set))
                   (memb? elem (cdr set)))
               #f))
          ( (eq? elem (car set)) #t)
          ( else (memb? elem (cdr set))))))

(define set-equal?
  (lambda (set1 set2)
    (cond ((and (null? set1) (null? set2)) #t)
          ((or (null? set1) (null? set2)) #f)
          ((not (and (list? set1) (list? set2))) #f)
          ((and (list? (car set1)) (list? (car set2)))
           (and (set-equal? (car set1) (car set2))
                 (set-equal? (cdr set1) (cdr set2))))
          ((or (list? (car set1)) (list? (car set2))) #f)
          ((and (eq? (car set1) (car set2))
                 (set-equal? (cdr set1) (cdr set2))) #t)
          (else #f))))

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

(define intersection
  (lambda (set1 set2)
    (if (or (null? set1) (null? set2))
         '()
         (let ((elem (car set1))
               (rest (cdr set1)))
           (if (memb? elem set2)
               (cons elem (intersection rest set2))
               (intersection rest set2))))))

(define difference
  (lambda (set1 set2)
    (if (or (null? set1) (null? set2))
        set1
        (let ((elem (car set1))
              (rest (cdr set1)))
          (if (memb? elem set2)
              (difference rest set2)
              (cons elem (difference rest set2)))))))