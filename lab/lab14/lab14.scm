(define (split-at lst n)
  (cond 
    ((null? lst) '(()))
    ((= n 0) (append '(()) lst))
    (else
     (define ls (split-at (cdr lst) (- n 1)))
     (cons (append (cons (car lst) nil) (car ls)) (cdr ls)))))

(define (compose-all funcs)
  (define (fx x)
    (if (null? funcs)
        x
        ((compose-all (cdr funcs)) ((car funcs) x))))
  fx)
