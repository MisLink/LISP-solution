#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))
(define (enumerate low high)
  (if (> low high)
      '()
      (cons low (enumerate (+ 1 low) high))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate 1 (- i 1))))
             (enumerate 1 n)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime? n)
  (define (next x)
    (if (odd? x)
        (+ x 2)
        (+ x 1)))
  (define (find-divisor r n)
    (cond ((> (* r r) n) n)
          ((= (remainder n r) 0) r)
          (else
           (find-divisor (next r) n))))
  (define (small-divisor n)
    (find-divisor 2 n))
  (= (small-divisor n) n))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))
(prime-sum-pairs 6)