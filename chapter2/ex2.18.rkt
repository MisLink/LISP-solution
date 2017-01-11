#lang racket
(define nil '())

(define (reverse items)
  (define (iter l result)
    (if (null? l)
        result
        (iter (cdr l) (cons (car l) result))))
  (iter items nil))

(reverse (list 1 2 3 4))
(list 1 2 3 4)
