#lang racket
(require "hw4.rkt")
(require rackunit)
(require rackunit/text-ui)

(define thlambda (lambda () 425))
(define ththunk (thunk 231))
(define thpoison (thunk (raise (error "never dethunk"))))

(define thunk-tests
  (test-suite
   "Thunk"
   
   (check-true (procedure? thlambda))
   (check-eq? (procedure-arity thlambda) 0)
   (check-true (thunk? thlambda))
   (check-eq? (thlambda) 425)
   (check-eq? (dethunk-that thlambda) 425)

   (check-true (procedure? ththunk))
   (check-eq? (procedure-arity ththunk) 0)
   (check-true (thunk? ththunk))
   (check-eq? (ththunk) 231)
   (check-eq? (dethunk-that ththunk) 231)

   (check-true (procedure? thpoison))
   (check-eq? (procedure-arity thpoison) 0)
   (check-true (thunk? thpoison))

   (check-exn exn:fail? (lambda () (dethunk-that thpoison)))

   ))

(run-tests thunk-tests)

(define (plausible-stream? s)
  (if (thunk? s)
      (let [(pr (s))]
        (if (pair? pr)
            (thunk? (cdr pr))
            #f
            ))
      #f))

(define ones-stream (lambda () (cons 1 ones-stream)))
(define ones-stream-prime (next-stream-from-stream ones-stream))
(define ones-stream-prime-prime (next-stream-from-stream ones-stream-prime))

; a stream which produces #t #f #t #f ...
(define flip-flop-stream
  (local [(define flip-stream (thunk-that (stream-cons-ensuring-stream-prime-is-thunk #t flop-stream)))
          (define flop-stream (thunk-that (stream-cons-ensuring-stream-prime-is-thunk #f flip-stream)))]
    flip-stream))

(define flip-flop-stream-prime (next-stream-from-stream flip-flop-stream))
(define flip-flop-stream-prime-prime (next-stream-from-stream flip-flop-stream-prime))
(define flip-flop-stream-prime-prime-prime (next-stream-from-stream flip-flop-stream-prime-prime))

(define stream-tests
  (test-suite
   "Stream"

   (check-true (plausible-stream? ones-stream))
   (check-eq? (value-from-stream ones-stream) 1)
   (check-true (plausible-stream? ones-stream-prime))
   (check-eq? (value-from-stream ones-stream-prime) 1)
   (check-true (plausible-stream? ones-stream-prime-prime))
   (check-eq? (value-from-stream ones-stream-prime-prime) 1)

   (check-false (plausible-stream? (thunk-that "this is not a steam")))
   (check-false (plausible-stream? (thunk-that (cons 425 "this is also not a steam"))))

   ; create a stream with thunk and stream-cons
   (check-true (plausible-stream? (thunk-that (stream-cons-ensuring-stream-prime-is-thunk 1 ones-stream))))

   ; NOTE: check-exn requires a thunk
   ; 2 is not a stream
   (check-exn exn:fail? (thunk-that (stream-cons-ensuring-stream-prime-is-thunk 1 2)))

   ;  ones-stream  is a stream
   ; (ones-stream) is NOT a stream. (ones-stream) is a pair.
   (check-exn exn:fail? (thunk-that (stream-cons-ensuring-stream-prime-is-thunk 1 (ones-stream))))

   (check-true (plausible-stream? flip-flop-stream))
   (check-true (value-from-stream flip-flop-stream))
   (check-true (plausible-stream? flip-flop-stream-prime))
   (check-false (value-from-stream flip-flop-stream-prime))
   (check-true (plausible-stream? flip-flop-stream-prime-prime))
   (check-true (value-from-stream flip-flop-stream-prime-prime))
   (check-true (plausible-stream? flip-flop-stream-prime-prime-prime))
   (check-false (value-from-stream flip-flop-stream-prime-prime-prime))
   )
  )

(run-tests stream-tests)
