#lang racket
(define (square tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else
         (cons (square (car tree)) (square (cdr tree))))))

(define (square1 tree)
  (map (lambda (sub)
         (if (pair? sub)
             (square1 sub)
             (* sub sub)))
       tree))
(square1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))
