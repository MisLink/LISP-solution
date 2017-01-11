;返回两个数中较大的
(define (larger x y)
  (if (< x y)
      y
      x))
;平方
(define (square x)(* x x))
;平方和
(define (sum-of-squares x y)
  (+ (square x) (square y)))
;三个数中较大的两个的平方和
(define (sum-of-larger-two-in-three x y z)
  (if (= x (larger x y))
      (sum-of-squares x (larger y z))
      (sum-of-squares y (larger x z))))

(sum-of-larger-two-in-three 5 3 2)
