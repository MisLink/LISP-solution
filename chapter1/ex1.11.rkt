;递归
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))
;迭代
(define (f2 n)
  (f-iter 2 1 0 0 n))
(define (f-iter a b c i n)
  (if (= i n)
      c
      (f-iter (+ a
                 (* 2 b)
                 (* 3 c))
              a
              b
              (+ 1 i) n)))
(f2 5)
