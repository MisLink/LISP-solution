(load "ex2.7.rkt")

(define (make-center-percent c p)
  (define percent (/ p 100.0))
  (make-interval (* c (- 1 percent)) (* c (+ 1 percent))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (* (/ (- (upper-bound i) (center i)) (center i)) 100))

(define x (make-center-percent 3 10))
(upper-bound x)
(percent x)