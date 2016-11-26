;递归
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))
;迭代
(define (product1 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))
;阶乘
(define (factorial n)
  (define (next x)
    (+ x 1))
  (define (term x)
    x)
  (product1 term 1 next n))
;(factorial 10)
;求PI
(define (pi n)
  (define (up x)
    (cond ((= x 1) 2)
          ((odd? x) (+ x 1))
          (else (+ x 2))))
  (define (down x)
    (if (odd? x)
        (+ x 2)
        (+ x 1)))
  (define (odd? x)
    (if (= (remainder x 2) 0)
        #f
        #t))
  (define (next x)
    (+ x 1))
  (* 4 (exact->inexact (/ (product1 up 1 next n) (product1 down 1 next n)))))
(pi 10000)