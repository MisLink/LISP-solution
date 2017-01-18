#lang racket
(define (make-accumulator value)
  (lambda (add-value)
    (set! value (+ value add-value))
    value))

(define A (make-accumulator 5))
(A 10)
(A 4)
(define B (make-accumulator 10))
(B 20)
