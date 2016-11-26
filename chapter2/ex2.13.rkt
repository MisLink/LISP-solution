(load "ex2.7.rkt")

(define (make-center-percent c p)
  (define percent (/ p 100.0))
  (make-interval (* c (- 1 percent)) (* c (+ 1 percent))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (* (/ (- (upper-bound i) (center i)) (center i)) 100))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define i (make-center-percent 10 0.5)) 
(define j (make-center-percent 10 0.4)) 
(percent (mul-interval i j))
