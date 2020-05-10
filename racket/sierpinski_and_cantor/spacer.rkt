#lang racket
(require 2htdp/image)
(provide (all-defined-out))

; fully transparent rectangle
(define (spacer-rect w h)
  (rectangle w h "outline" (color 0 0 0 0)))

; fully transparent image the size of img
(define (spacer-image img)
  (spacer-rect (image-width img) (image-height img)))
