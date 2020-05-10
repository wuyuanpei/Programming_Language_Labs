;; Programming Languages, Homework 5 Bonus

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

(require "hw5.rkt")

; Richard Wu

; racket version for reference
(define (racket-double n) (+ n n))
; mupl-double return a mupl-function which doubles its mupl-int argument
(define mupl-double 
         (fun #f "n" (add (var "n") (var "n")))) 

; racket version for reference
(define (racket-sum-curry a) (lambda (b) (+ a b)))
; mupl-sum-curry return a mupl-function which returns a mupl-function which adds the two mupl-function's mupl-int arguments together
(define mupl-sum-curry
        (fun #f "a"
             (fun #f "b"
                  (add (var "a") (var "b")))))

; racket version for reference
(define (racket-map-one proc) (proc 1))
; mupl-map-one: return a mupl-function that invoks the mupl-function passed in with the mupl-int argument 1
(define mupl-map-one
        (fun #f "proc"
             (call (var "proc") (int 1))))

