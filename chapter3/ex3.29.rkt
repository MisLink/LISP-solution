#lang planet neil/sicp
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input))

(define (logical-not x)
  (cond ((= x 1) 0)
        ((= x 0) 1)
        (else
         (error "error signal"))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a1 and-action-procedure))
(define (or-gate a1 a2 output)
  (let ((invert1 (make-wire))
        (invert2 (make-wire))
        (and-invert (make-wire)))
    (inverter a1 invert1)
    (inverter a2 invert2)
    (and-gate invert1 invert2 and-invert)
    (inverter and-invert output)))
