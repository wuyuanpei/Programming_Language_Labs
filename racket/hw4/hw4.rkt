
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; Richard Wu

; Complete WashU Thunk and Stream WarmUp Below

; CSE 425 Utility Function
(define (thunk? th) 
        (and (procedure? th) (= (procedure-arity th) 0))) 

; CSE 425 Utility Macro
; NOTE: macros use define-syntax-rule
(define-syntax-rule (thunk-that e)
        (lambda () e)) 

; CSE 425 Utility Function
(define (dethunk-that e)
        (if (thunk? e) (e) (raise (error "not a thunk" e)))) 

; CSE 425 Utility Function
(define (value-next-stream-pair-from-stream s)
        (let ([p (dethunk-that s)])
        (values (car p) (cdr p))))

; CSE 425 Utility Function
(define (value-from-stream s)
  (let-values ([(value s-prime) (value-next-stream-pair-from-stream s)])
    value))

; CSE 425 Utility Function
(define (next-stream-from-stream s)
  (let-values ([(value s-prime) (value-next-stream-pair-from-stream s)])
    s-prime))

; CSE 425 Utility Function
(define (stream-cons-ensuring-stream-prime-is-thunk value s-prime)
        (if (thunk? s-prime) (cons value s-prime) (raise (error "not a thunk" s-prime)))) 


; Complete UW HW4 Below

(define (sequence low high stride)
  (if (> low high) null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (if (< n 0) (error "list-nth-mod: negative number")
      (if (null? xs) (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

(define (stream-for-n-steps s n)
  (if (= n 0) null
      (cons (value-from-stream s) (stream-for-n-steps (next-stream-from-stream s) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x) (cons x (lambda () (f (if (= (remainder x 5) 4) (- 0 (+ x 1))  (+ (abs x) 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (cons x (lambda () (f (if (string=? x "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (letrec ([f (lambda (x ns) (cons (cons 0 (value-from-stream s)) ns))])
    (lambda () (f s (stream-add-zero (next-stream-from-stream s))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (n) (if (= n (vector-length vec)) #f
                              (let ([e (vector-ref vec n)])
                                (if (pair? e)
                                    (if (equal? v (car e))
                                        e
                                        (f (+ n 1)))
                                    (f (+ n 1))))))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n)]
           [index 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                 (if ans 
                     (cdr ans)
                     (let ([new-ans (assoc v xs)])
                       (begin
                         (vector-set! memo index (cons v new-ans))
                         (set! index (if (= index (- n 1)) 0 (+ index 1)))
                         new-ans)))))])
    f))

(define-syntax while-less 
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([x e1]
              [y e2]
              [f (lambda () (if (> x y) (begin (set! y e2) (f))
                                #t))])
       (f)
       )]))