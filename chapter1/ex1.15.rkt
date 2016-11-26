(#%require racket/trace)
(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))
(trace p)
(sine 12.15);5次
;O(log a)