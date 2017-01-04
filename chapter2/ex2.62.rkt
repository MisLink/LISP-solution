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

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (cond ((= (car set1) (car set2))
                (cons (car set1) (union-set (cdr set1) (cdr set2))))
               ((< (car set1) (car set2))
                (cons (car set1) (union-set (cdr set1) set2)))
               (else
                (cons (car set2) (union-set set1 (cdr set2))))))))


(union-set (list 1 2) (list 1 3 5 7 9))
(union-set '() (list 1 2 3))
(union-set '(1 2) '())
(union-set (list 1 2 3) (list 1 3 5))
(union-set (list 1 2 3) (list 1 3 5 7 9))