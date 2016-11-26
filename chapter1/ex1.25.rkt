(define (square x) (* x x))
(define (even? x)
  (if (= (remainder x 2) 0)
      true
      false))

(define (fast-expt b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt(square b) (/ n 2) a))
        (else (fast-expt b (- n 1) (* a b)))))
(define (expt b n)
  (fast-expt b n 1))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (expmod1 base exp m)
    (remainder (expt base exp) m))
