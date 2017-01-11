#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (col) (dot-product col v)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (col-of-m)
           (matrix-*-vector cols col-of-m))
         m)))

(define m (list (list 1 2 3 4)
                (list 4 5 6 6)
                (list 6 7 8 9)))
(define n (list (list 1 4 6)
                (list 2 5 7)
                (list 3 6 8)
                (list 4 6 9)))
(define v (list 1 2 3 4))

(dot-product (list 1 2 3) (list 1 2 3))
(matrix-*-vector m v)
(matrix-*-matrix m n)
