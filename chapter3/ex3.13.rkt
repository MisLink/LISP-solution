#lang planet neil/sicp
(define (make-cricle x)
  (set-cdr! (last-pair x) x)
  x)
(define (last-pair x)
  (if (null? (cdr x))
             x
             (last-pair (cdr x))))
(define x (list 1 2 3))
(define z (make-cricle x))
(last-pair z)
