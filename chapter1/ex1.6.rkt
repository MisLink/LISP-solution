;平均值
(define (average x y)
  (/ (+ x y) 2))
;修改guess
(define (improve guess x)
  (average guess (/ x guess)))
;是否符合要求
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
;平方
(define (square x)(* x x))
;绝对值
(define (abs a)
  (if (< a 0)
      (- a)
      a))
;new-if 应用序会对两个cond的表达式求值，而if语句只对判断为真的语句求值
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
