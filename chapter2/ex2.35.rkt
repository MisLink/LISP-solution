#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (pair? x)
                             (count-leaves x)
                             1)) t)))

(count-leaves (list (list 1 2) (list 3 4)))
(count-leaves (list (list 1 (list 2 3)) (list (list 4 5) (list 6 7))))
