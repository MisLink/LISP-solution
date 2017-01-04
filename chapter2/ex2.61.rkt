#lang racket
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else
         (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (cond ((= x (car set))
             set)
            ((> x (car set))
             (cons (car set) (adjoin-set x (cdr set))))
            (else
             (cons x set)))))

(adjoin-set 1 '())
(define set1 (adjoin-set 3 (list 2 3)))
(define set2 (adjoin-set 1.6 (list 1 2 3)))
(intersection-set set1 set2)

