#lang racket
(define x (list (list 1 2) (list 3 4)))
(define nil '())
(define (reverse l)
  (define (iter x result)
    (if (null? x)
        result
        (iter (cdr x) (cons (car x) result))))
  (iter l nil))

(define (deep-reverse l)
  (cond ((null? l)
         '())
        ((or (not (pair? l)) (null? (cdr l)))
         l)
        (else
         (reverse (list (deep-reverse (car l)) (deep-reverse (cadr l)))))))
(deep-reverse x)
(reverse x)