#lang planet neil/sicp

(define (loop? lst)
  (define (iter x y)
    (let ((fast (list-walk 2 x))
          (slow (list-walk 1 y)))
      (cond ((or (null? fast) (null? slow))
             false)
            ((eq? fast slow)
             true)
            (else
             (iter fast slow)))))
  (iter lst lst))
(define (list-walk step x)
  (cond ((null? x)
         '())
        ((= step 0)
         x)
        (else
         (list-walk (- step 1) (cdr x)))))

(define cycle (list 1 2 3))
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(set-cdr! (last-pair cycle) cycle)
(define x (list 1 2 3))
(loop? cycle)
(loop? x)
