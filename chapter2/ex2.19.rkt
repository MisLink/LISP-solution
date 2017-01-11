#lang racket
(define (no-more? lst)
  (if (null? lst)
      true
      false))
(define (except-first-denomination lst)
  (cdr lst))
(define (first-denomination lst)
  (car lst))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values)) coin-values)))))

(cc 100 (list 50 25 10 5 1))
