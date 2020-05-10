#lang racket
(require 2htdp/image)
(provide (all-defined-out))

; Richard Wu
(define (tri-bar-flag/left-to-right width height left-color middle-color right-color)
   (beside (rectangle (/ width 3) height "solid" left-color)
           (rectangle (/ width 3) height "solid" middle-color)
           (rectangle (/ width 3) height "solid" right-color)))

(define (tri-bar-flag/top-to-bottom width height top-color middle-color bottom-color)
    (above (rectangle width (/ height 3) "solid" top-color)
           (rectangle width (/ height 3) "solid" middle-color)
           (rectangle width (/ height 3) "solid" bottom-color)))

(define (france-flag w h)
  (tri-bar-flag/left-to-right w h "blue" "white" "red"))

(define (netherlands-flag w h)
  (tri-bar-flag/top-to-bottom w h "red" "white" "blue"))

(define (ireland-flag w h)
  (tri-bar-flag/left-to-right w h "green" "white" "orange"))

(define (india-flag w h)
  (overlay
   (circle (/ h 30) "solid" "DarkBlue")
   (radial-star 32 (/ h 30) (/ h 10) "solid" "DarkBlue")
   (circle (/ h 10) "solid" "White") ;todo
   (circle (/ h 8) "solid" "DarkBlue")
   (tri-bar-flag/top-to-bottom w h "orange" "white" "green")))

(module+ main ; evualated when enclosing module is run directly (that is: not via require)
  (france-flag 300 200)
  (netherlands-flag 300 200)
  (ireland-flag 300 150)
  (india-flag 300 200)
)
