#lang racket
(require 2htdp/image)
(provide (all-defined-out))

; Richard Wu
(define (home roof-side-length roof-color base-width base-height base-color door-width door-height door-color)
   (above (triangle roof-side-length "solid" roof-color)
          (overlay/align "middle" "bottom"
                         (rectangle door-width door-height "solid" door-color)
                         (rectangle base-width base-height "solid" base-color))))

(module+ main ; evualated when enclosing module is run directly (that is: not via require)
  (home 125 "green" 100 100 "brown" 25 50 "yellow")
)
