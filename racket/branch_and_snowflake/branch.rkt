#lang racket
(require 2htdp/image)
(provide (all-defined-out))

; Richard Wu
(define (branch length iter) 
        (if (= iter 0) (line length 0 "green")
            (above/align "left"
                         (overlay/align/offset "left" "bottom"
                                               (rotate 30 (branch (/ length 2) (- iter 1)))
                                               (- (* 2 (/ length 3)))
                                               0
                                               (line length 0 "brown"))
                         (overlay/align/offset "left" "top"
                                               (rotate 330 (branch (/ length 2) (- iter 1)))
                                               (- (/ length 3))
                                               0
                                               empty-image))))
                                  
                                  
                               
             
             

(module+ main ; evualated when enclosing module is run directly (that is: not via require)
  (for-each
   displayln
   (local
     ([define length 400])
     (for/list ([iter (in-range 8)])
       (branch length iter))))
)
