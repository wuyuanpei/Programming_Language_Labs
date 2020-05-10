#lang racket
(require 2htdp/image)
(provide (all-defined-out))

; Richard Wu

(define (heart side-length color)
  (rotate 45
  (overlay/xy  (overlay/xy (circle (/ side-length 2) "solid" color)
                          0
                          (/ side-length 2)
                          (rectangle side-length side-length "solid" color))
               
               (/ side-length 2)
               (/ side-length 2)
               (circle (/ side-length 2) "solid" color))))


(module+ main ; evualated when enclosing module is run directly (that is: not via require)
  (heart 200 "red")
)
