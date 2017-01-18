#lang racket
(define (make-account balance password)
  (define (withdraw amount pwd)
    (if (equal? pwd password)
        (if (>= balance amount)
            (begin
              (set! balance (- balance amount))
              balance)
            "error!")
        "error!"))
  withdraw)

(define A (make-account 100 'guo))
(A 10 'guo)
(A 110 'guo)