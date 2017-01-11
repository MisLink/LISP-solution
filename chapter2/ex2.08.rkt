(load "ex2.7.rkt")
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (add-interval x
                 (make-interval (- (upper-bound y)) (- (lower-bound y)))))

(sub-interval (make-interval 5 6) (make-interval 3 4))
