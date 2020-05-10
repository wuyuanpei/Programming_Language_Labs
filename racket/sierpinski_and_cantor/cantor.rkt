#lang racket
(require 2htdp/image)
(require "spacer.rkt")
(provide (all-defined-out))

(define (brick w h)
  (overlay
   (rectangle w h "outline" "blue")
   (rectangle w h "solid" "red")
   )
  )

; Richard Wu
(define (cantor-stool width height n)
  (if (= n 0) (brick width height)
      (above (brick width (/ height 2))
             (if (= n 1)
                 (beside (brick (/ width 3) (/ height 2))
                         (rectangle (/ width 3) (/ height 2) "solid" "white")
                         (brick (/ width 3) (/ height 2)))
                 (let ([half (cantor-stool (/ width 3) (/ height 2) (- n 1))])
                   (beside half (rectangle (/ width 3) (/ height 2) "solid" "white") half))))))

(module+ main ; evualated when enclosing module is run directly (that is: not via require)
  (cantor-stool 400 400 0)
  (cantor-stool 400 400 1)
  (cantor-stool 400 400 2)
  (cantor-stool 400 400 3)
  (cantor-stool 400 400 4)
)
