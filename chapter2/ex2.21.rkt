#lang racket
(define nil '())
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))
(define (square-list1 items)
  (map (lambda (x) (* x x)) items))
(square-list1 (list 1 2 3 4))