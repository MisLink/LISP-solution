#lang planet neil/sicp
(define (or-gate a1 a2 output)
  (define (and-action-procdure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procdure)
  (add-action! a1 and-action-procdure))
(define (logical-or x y)
  (if (or (= x 1) (= y 1))
      1
      0))
