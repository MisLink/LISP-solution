(#%require racket/trace)
(define (double f)
  (lambda (x)
    (f (f x))))

(define (inc x)
  (+ x 1))
(trace inc)
(((double (double double)) inc) 5)
