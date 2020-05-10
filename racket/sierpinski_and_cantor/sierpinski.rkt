#lang racket
(require 2htdp/image)
(require "spacer.rkt")
(provide (all-defined-out))

; Richard Wu
(define (sierpinski-triangle side-length n)
  (if (= n 0)
      (triangle side-length "solid" "blue")
      (let ([half (sierpinski-triangle (/ side-length 2) (- n 1))])
        (overlay/align "left" "bottom"
                       half
                       (overlay/align "right" "bottom"
                                      half
                                      (overlay/align "middle" "top"
                                                     half
                                                     (overlay/align "middle" "bottom"
                                                                    (rotate 180 (triangle (/ side-length 2) "solid" "white"))
                                                                    (triangle side-length "solid" "blue"))))))))

(define (sierpinski-carpet side-length n)
  (if (= n 0) (rectangle side-length side-length "solid" "orange")
      (let ([half (sierpinski-carpet (/ side-length 3) (- n 1))])
  (overlay/align "right" "bottom"
                 half
                 (rectangle (/ side-length 3) (/ side-length 3) "outline" "white")
  (overlay/align "left" "bottom"
                 half
                 (rectangle (/ side-length 3) (/ side-length 3) "outline" "white")
  (overlay/align "right" "top"
                 half
                 (rectangle (/ side-length 3) (/ side-length 3) "outline" "white")
  (overlay/align "left" "top"
                 half
                 (rectangle (/ side-length 3) (/ side-length 3) "outline" "white")
  (overlay/align "middle" "bottom"
                 half
                 (rectangle (/ side-length 3) (/ side-length 3) "outline" "white")
  (overlay/align "middle" "top"
                 half
                 (rectangle (/ side-length 3) (/ side-length 3) "outline" "white")
  (overlay/align "left" "middle"
                 half
                 (rectangle (/ side-length 3) (/ side-length 3) "outline" "white")
  (overlay/align "right" "middle"
                 half
                 (rectangle (/ side-length 3) (/ side-length 3) "outline" "white")
  (overlay/align "middle" "middle"
                 (rectangle (/ side-length 3) (/ side-length 3) "solid" "white")
                 (rectangle side-length side-length "solid" "orange")))))))))))))


(module+ main ; evualated when enclosing module is run directly (that is: not via require)
  (sierpinski-triangle 400 0)
  (sierpinski-triangle 400 1)
  (sierpinski-triangle 400 2)
  (sierpinski-triangle 400 3)
  (sierpinski-triangle 400 4)

  (sierpinski-carpet 400 0)
  (sierpinski-carpet 400 1)
  (sierpinski-carpet 400 2)
  (sierpinski-carpet 400 3)
  (sierpinski-carpet 400 4)
)
