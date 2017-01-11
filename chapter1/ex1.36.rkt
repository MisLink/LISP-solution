(define tolerance 0.000001)
(define (display-info info step)
  (display "Step: ")
  (display step)
  (display " ")
  (display "Guess: ")
  (display info)
  (newline))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess step)
    (display-info guess step)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          (begin
            (display-info next (+ 1 step))
            next)
          (try next (+ step 1)))))
  (try first-guess 1))

(define formula
  (lambda (x) (/ (log 1000) (log x))))
(define formula-avg
  (lambda (x) (/ (+ x (formula x)) 2)))
(fixed-point formula-avg 2.0)
