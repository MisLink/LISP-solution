(define (cbrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      (improve guess x)
      (cbrt-iter (improve guess x) x)))
;更新猜测值
(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))
;平方
(define (square x)
  (* x x))
;立方
(define (cube x)
  (* x x x))
;判断是否符合标准
(define (good-enough? guess next-guess)
  (< (/ (abs (- guess next-guess)) guess) 0.001))
;绝对值
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (cbrt x)
  (cbrt-iter 1.0 x))
(cbrt 0.00009)