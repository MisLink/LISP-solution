#lang racket
(define (last-pair l)
  (cond ((null? l)
         (error "list empty"))
        ((null? (cdr l))
         l)
        (else
         (last-pair (cdr l)))))

(last-pair (list 1 2 3 4))
  
