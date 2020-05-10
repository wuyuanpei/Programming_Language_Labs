#lang racket

(define (*2 v)
  (+ v v))

(define powers-of-2 (stream-cons 1 (stream-map *2 powers-of-2)))

;(stream? powers-of-2)
;(stream-first powers-of-2)
;(stream-first (stream-rest powers-of-2))
;(stream-first (stream-rest (stream-rest powers-of-2)))

(define first-10-powers-of-2 (stream-take powers-of-2 10))
(stream->list first-10-powers-of-2)

(define (reverse-stream s)
  (stream* (reverse (stream->list s))))

(define reversed-first-10-powers-of-2 (reverse-stream first-10-powers-of-2))

(stream->list reversed-first-10-powers-of-2)

(define (reciprocal v)
  (/ 1 v))

(define negative-powers-of-2 (stream-map reciprocal (stream-rest powers-of-2)))

;(stream-first negative-powers-of-2)
(stream->list (stream-take negative-powers-of-2 10))