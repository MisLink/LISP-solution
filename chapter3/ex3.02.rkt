#lang racket
(define (make-monitored f)
  (let ((count-times 0))
    (lambda (input)
      (cond ((eq? input 'how-many-calls?) count-times)
            ((eq? input 'reset)
             (begin (set! count-times 0)
                    count-times))
            (else
             (begin
               (set! count-times (+ count-times 1))
               (f input)))))))

(define A (make-monitored sqrt))
(A 100)
(A 'how-many-calls?)
