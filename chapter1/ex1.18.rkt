(define (double x)
  (+ x x))

(define (even? x)
  (= (remainder x 2) 0))

(define (halve x)
  (cond ((even? x) (/ x 2))))

(define (multi-iter b n x)
  (cond ((= n 0) x)
        ((even? n) (multi-iter (double b) (halve n) x))
        (else (multi-iter b (- n 1) (+ b x)))))
(define (multi b n)
  (multi-iter b n 0))
(multi 4 5)