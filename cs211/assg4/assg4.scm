(define memb?
  (lambda (elem set)
    (cond ( (null? set) #f)
          ( (list? elem) (if (list? (car set)) 
                             (and (set-equal? elem (car set)
                                  (memb? elem (cdr set)))) 
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
          
