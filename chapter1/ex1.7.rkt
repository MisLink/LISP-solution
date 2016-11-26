;平均值
(define (average x y)
  (/ (+ x y) 2))
;平方
(define (square x)(* x x))
;绝对值
(define (abs a)
  (if (< a 0)
      (- a)
      a))
;更新猜测值
(define (improve guess x)
  (average guess (/ x guess)))
;判断是否符合标准
(define (good-enough? guess next-guess)
  (< (/ (abs (- next-guess guess)) guess) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      (improve guess x)
      (sqrt-iter (improve guess x) x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(sqrt 0.00009)