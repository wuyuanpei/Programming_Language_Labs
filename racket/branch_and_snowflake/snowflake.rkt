#lang racket
(require 2htdp/image)
(provide (all-defined-out))

; Richard Wu
(define (snowflake len iter is-flipped)
  (if (= iter 0) (line len 0 "blue")
      (beside/align "bottom"
                    (snowflake (/ len 3) (- iter 1) is-flipped)
                    (rotate 60 (snowflake (/ len 3) (- iter 1) is-flipped))
                    (if is-flipped
                        (rotate 120 (snowflake (/ len 3) (- iter 1) is-flipped))
                        (rotate 300 (snowflake (/ len 3) (- iter 1) is-flipped)))
                    (snowflake (/ len 3) (- iter 1) is-flipped))))

(define (snowflake-symmetric len iter)
  (snowflake len iter #f))

(define (snowflake-flipped len iter)
  (snowflake len iter #t))

(module+ main ; evualated when enclosing module is run directly (that is: not via require)
  (for-each
   displayln
   (local
     ([define length 400])
     (append
      (for/list ([iter (in-range 5)])
        (snowflake-symmetric length iter))
      (for/list ([iter (in-range 5)])
        (snowflake-flipped length iter))))))
