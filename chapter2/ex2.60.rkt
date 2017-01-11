#lang racket

(define (element-of-set? x set)
  (cond ((null? set)
         false)
        ((equal? x (car set))
         true)
        (else
         (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (define (iter set result)
    (if (or (null? set) (null? set2))
        (reverse result)
        (if (and (element-of-set? (car set) set2) (not (element-of-set? (car set) result)))
            (iter (cdr set) (cons (car set) result))
            (iter (cdr set) result))))
    (iter set1 '()))
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (append set1 set2))))

(intersection-set (list 1 2 3) (list 1 2 3))
(intersection-set (list 1 2 1 2) (list 1 2 1 2))
