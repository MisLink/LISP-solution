;递归
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))
;迭代
(define (accumulate1 combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
    (iter (next a) (combiner result (term a)))))
  (iter a null-value))
;求和
(define (sum a b)
  (define (term x)
    x)
  (define (next x)
    (+ x 1))
  (accumulate1 + 0 term a next b))
;求积
(define (product a b)
  (define (term x) x)
  (define (next x) (+ x 1))
  (accumulate1 * 1 term a next b))
(product 1 10)
