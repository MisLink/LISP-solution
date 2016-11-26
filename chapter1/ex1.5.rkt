;应用序会先求值，这时调用p会产生无限循环；正则序会先完全展开，不会调用到p，会输出0。
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
(test 0 (p))