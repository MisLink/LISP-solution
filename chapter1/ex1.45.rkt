(define (repeated f n)
  (define (iter i ff)
    (if (= i 1)
        ff
        (iter (- i 1) (lambda (x) (f (ff x))))))
  (iter n f))

(define (expt base n)
  (if (= n 0)
      1
      ((repeated (lambda (x) (* base x)) n) 1)))

(define (average a b)
  (/ (+ a b) 2.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (log2 n)
  (/ (log n) (log 2)))

(define (average-dump-n-times n f)
  ((repeated average-damp n) f))

(define (nth-root n x)
  (fixed-point (average-dump-n-times (floor (log2 n))
                (lambda (y)
                  (/ x (expt y (- n 1)))))
               1.0))

(nth-root 4 81)
