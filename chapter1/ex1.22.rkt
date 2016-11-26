;素数
(#%require racket)
(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x) (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

;
(define (next-odd n)
  (if (= (remainder n 2) 1)
      (+ 2 n)
      (+ 1 n)))

(define (continue-primes n count)
  (cond ((= count 0) (newline))
        ((prime? n) (display n)
                    (newline)
                    (continue-primes (next-odd n) (- count 1)))
        (else (continue-primes (next-odd n) count))))

(define (search-for-primes n)
  (define start-time (current-milliseconds))
  (continue-primes n 3)
  (- (current-milliseconds) start-time))
(search-for-primes 4000000000)