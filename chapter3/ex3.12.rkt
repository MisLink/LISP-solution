#lang planet neil/sicp
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
x
y
(cdr x)
(define (append! x y)
  (set-cdr! (last-pair x) y))
(define (last-pair x)
  (if (null? (cdr x))
             x
             (last-pair (cdr x))))
(append! x y)
x
y
(cdr x)
