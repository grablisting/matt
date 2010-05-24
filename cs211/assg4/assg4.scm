(define memb?
  (lambda (elem set)
    (cond ( (null? set) #f)
          ( (list? elem) (if (list? (car set)) 
                             (set-equal? elem (car set)) 
                             #f))
          ( (eq? elem (car set)) #t)
          ( else (memb? elem (cdr set))))))

(define xor
  (lambda (t1 t2)
    (if (or (and t1 t2) (and (not t1) (not t2)))
        (#f)
        (#t))))

(define set-equal?
  (lambda (set1 set2)
    (cond ( (or (null? set1) (null? set2)) #f)
          ( (not (xor (list? set1) (list? set2))) #f)
          

