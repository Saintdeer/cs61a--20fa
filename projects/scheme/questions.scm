(define (caar x) (car (car x)))

(define (cadr x) (car (cdr x)))

(define (cdar x) (cdr (car x)))

(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement
(define (zip pairs)
    (list (map car pairs) (map cadr pairs)))

(define (zip2 pairs)
  (if (null? (car pairs))
      nil                                       ; (pairs)            --> ((1 2) (3 4)) 
      (cons (list (caar pairs)                  ; (caar pairs)       -->   1
                  (caar (cdr pairs)))           ; (caar (cdr pairs)) -->         3
            (zip2
             (list (cdar pairs)                 ; (cdar pairs)       -->     2
                   (cdar (cdr pairs)))))))      ; (cdar (cdr pairs)) -->           4


(zip2 '((1 2 3) (4 5 6)))
; expect ((1 4) (2 5) (3 6))

(define (length_to_list start end)
  (if (= start end)
      nil
      (cons start (length_to_list (+ start 1) end))))

(length_to_list 0 3)
; expect (0 1 2)

; ; Problem 15
; ; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  (if (null? s)
      nil
      (zip2 (list (length_to_list 0 (length s)) s))))
  ; END PROBLEM 15
  
; ; Problem 16
; ; Merge two lists LIST1 and LIST2 according to COMP and return
; ; the merged lists.
(define (merge comp list1 list2)
  ; BEGIN PROBLEM 16
  (cond 
    ((null? list1)
     list2)
    ((null? list2)
     list1)
    ((comp (car list1) (car list2))
     (cons (car list1) (merge comp (cdr list1) list2)))
    (else
     (cons (car list2) (merge comp list1 (cdr list2))))))
  ; END PROBLEM 16
  
(merge < '(1 5 7 9) '(4 8 10))
; expect (1 4 5 7 8 9 10)

(merge > '(9 7 5 1) '(10 8 4 3))
; expect (10 9 8 7 5 4 3 1)

; ; Problem 17
(define (nondecreaselist s)                           
  ; BEGIN PROBLEM 17                                
  (cond                                         
      ((null? (cdr s)) (list s))                             
      ((<= (car s) (cadr s)) (define lst (nondecreaselist (cdr s))) (cons (cons (car s) (car lst)) (cdr lst)))
      (else (cons (list (car s)) (nondecreaselist (cdr s))))))
  ; END PROBLEM 17

; ; Problem EC
; ; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))

(define define? (check-special 'define))

(define quoted? (check-special 'quote))

(define let? (check-special 'let))

; ; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond 
    ((atom? expr)
     ; BEGIN PROBLEM EC
     expr
     ; END PROBLEM EC
    )
    ((quoted? expr)
     ; BEGIN PROBLEM EC
     expr
     ; END PROBLEM EC
    )
    ((or (lambda? expr) (define? expr))
     (let ((form (car expr))
           (params (cadr expr))
           (body (cddr expr)))
       ; BEGIN PROBLEM EC
       (append (list form params) (map let-to-lambda body))
       ; END PROBLEM EC
     ))
    ((let? expr)
     (let ((values (cadr expr))
           (body (cddr expr)))
       ; BEGIN PROBLEM EC
       (define param_vals (zip values))
       (define params (car param_vals))
       (define vals (map let-to-lambda (cadr param_vals)))
       (define body (map let-to-lambda body))
       (cons (append (list 'lambda params) body) vals)
       ; END PROBLEM EC
     ))
    (else
     ; BEGIN PROBLEM EC
     (map let-to-lambda expr)
     ; END PROBLEM EC
    )))
