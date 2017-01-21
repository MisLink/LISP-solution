#lang racket

(define random-init
  (inexact->exact (current-milliseconds)))

(define (random-update x)
  (let ((m 9)
        (a 3)
        ( c 5))
    (modulo (+ (* a x) c) m)))

(define (rand m)
  (let ((x random-init))
    (cond ((eq? m 'generate)
           (begin
             (set! x (random-update x))
             x))
          ((eq? m 'reset)
           (lambda (value)
             (set! x value)
             x)))))


(rand 'generate)
