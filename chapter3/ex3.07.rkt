#lang racket
(define (make-account balance password)
  (define (withdraw amount pwd)
    (if (equal? pwd password)
        (if (>= balance amount)
            (begin
              (set! balance (- balance amount))
              balance)
            "error!")
        "pwd error!"))
  withdraw)

(define (make-joint account origin-password another-password)
  (lambda (amount password)
    (cond ((eq? password another-password)
           (account amount origin-password))
          (else
           "pwd-2 error!"))))

(define A (make-account 100 'guo))
(A 10 'guo)
(define B (make-joint A 'guo 'jia))
(B 20 'jia)
