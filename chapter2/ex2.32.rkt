#lang racket
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rset (subsets (cdr s))))
        (append rset (map (lambda (x)
                            (cons (car s) x))
                          rset)))))
(subsets (list 1 2 3))
