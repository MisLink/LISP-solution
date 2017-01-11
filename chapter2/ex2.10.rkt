(load "ex2.9.rkt")

(define i (make-interval 2 7))
(define j (make-interval 8 3))
(define span-0 (make-interval -1 1))

(define (div-interval x y)
  (if (< (* (lower-bound y) (upper-bound y)) 0)
      (error "Division error (interval spans 0)" y)
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

(mul-interval i j)
(mul-interval j i)
(div-interval i j)
(div-interval j span-0)
