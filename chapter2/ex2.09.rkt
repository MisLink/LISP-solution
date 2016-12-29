(define (make-interval a b)
  (cons a b))
(define (lower-bound x)
  (min (car x) (cdr x)))
(define (upper-bound x)
  (max (car x) (cdr x)))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (add-interval x
                 (make-interval (- (upper-bound y)) (- (lower-bound y)))))
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))

;(width (mul-interval (make-interval 2 6) (make-interval 3 4)))

