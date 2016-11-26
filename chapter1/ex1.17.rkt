(define (double x)
  (+ x x))

(define (even? x)
  (= (remainder x 2) 0))

(define (halve x)
  (cond ((even? x) (/ x 2))))

(define (multi-iter b n)
  (cond ((= n 0) 0)
        ((even? n) (double (multi-iter b (halve n))))
        (else (+ b (multi-iter b (- n 1))))))
(multi-iter 6 4)
