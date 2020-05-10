#lang racket
;; Programming Languages Homework 5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and, if necessary, change the filename
(require "hw5.rkt")
(require "mupl_programs.rkt")

(require rackunit)

(define tests
  (test-suite
   "MUPL Programs"
   
   (check-equal? (racket-double 7) 14 "racket-double test")
   (check-equal? (eval-exp (call mupl-double (int 7))) (int 14) "mupl-double test")

   (check-equal? ((racket-sum-curry 400) 25) 425 "racket-sum-curry test")
   (check-equal? (eval-exp (call (call mupl-sum-curry (int 400)) (int 25))) (int 425) "mupl-sum-curry test")

   (check-equal? (racket-map-one (lambda (n) (+ 1000 n))) 1001 "racket-map-one test")
   (check-equal? (eval-exp (call mupl-map-one (fun #f "x" (add (var "x") (int 1000))))) (int 1001) "mupl-map-one test")
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
