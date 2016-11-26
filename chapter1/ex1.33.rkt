;递归
(define (filter-accumulate combiner null-value term a next b valid?)
  (cond ((> a b) null-value)
        ((valid? a)
         (combiner (term a)
                   (filter-accumulate combiner null-value term (next a) next b valid?)))
        (else (filter-accumulate combiner null-value term (next a) next b valid?))))
;迭代
(define (filter-accumulate1 combiner null-value term a next b valid?)
  (define (iter a result)
    (cond ((> a b) result)
          ((valid? a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))
;素数
(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  
  (define (square x) (* x x))
  
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  
  (define (divides? a b)
    (= (remainder b a) 0))
  (= (smallest-divisor n) n))
;互素
(define (coprime? a b)
  (define (gcd a b)
    (if (= b 0) a
        (gcd b (remainder a b))))
  (and (< a b)
       (= 1 (gcd a b))))

(define (prime-sum a b)
  (filter-accumulate + 0 (lambda (x) x) a (lambda (i) (+ i 1)) b prime?))
;(prime-sum 1 10)
(define (coprime-product n)
  (define (coprime-n? x)
    (coprime? x n))
  (filter-accumulate1 * 1 (lambda (x) x) 1 (lambda (i) (+ i 1)) n coprime-n?))
(coprime-product 10)