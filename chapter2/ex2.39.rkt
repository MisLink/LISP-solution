#lang racket
(define (fold-left op initial sequence)
  (define (iter result rset)
    (if (null? rset)
        result
        (iter (op result (car rset)) (cdr rset))))
  (iter initial sequence))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
(define (reverse2 sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))
(reverse (list 1 2 3))
(reverse2 (list 1 2 3))
