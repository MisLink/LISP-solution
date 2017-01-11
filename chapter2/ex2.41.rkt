#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))
(define (enumerate low high)
  (if (> low high)
      '()
      (cons low (enumerate (+ 1 low) high))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate 1 (- i 1))))
             (enumerate 1 n)))

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (cons i j))
                  (unique-pairs (- i 1))))
           (enumerate 1 n)))

(define (equal? s triples)
  (= s (+ (car triples) (cadr triples) (caddr triples))))

(define (func s n)
  (filter (lambda (triples)
            (equal? s triples)) (unique-triples n)))
(func 10 13)
