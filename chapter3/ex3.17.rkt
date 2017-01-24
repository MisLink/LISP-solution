#lang planet neil/sicp
(define (false? x)
  (if (not x)
      true
      false))
(define (count-pairs x)
  (define (iter x visited)
    (if (and (pair? x) (false? (memq x visited)))
        (iter (car x)
              (iter (cdr x) (cons x visited)))
        visited))
  (length (iter x '())))

(define three (cons (cons 1 '()) (cons 2 '())))
three
(count-pairs three)
(define two (list 1 2))
(define four (cons two (cdr two)))
four
(count-pairs four)
(define one (list 1))
(define three_ (cons one one))
(define seven (cons three_ three_))
seven
(count-pairs seven)
(define cycle (list 1 2 3))
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(set-cdr! (last-pair cycle) cycle)
cycle
(count-pairs cycle)
