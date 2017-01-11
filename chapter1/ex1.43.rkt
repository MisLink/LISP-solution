;递归
(define (repeated f n)
  (if (= n 1)
      f
      (lambda (x)
        (f ((repeated f (- n 1)) x)))))
;迭代
(define (repeated1 f n)
  (define (iter i ff)
    (if (= i 1)
        ff
        (iter (- i 1) (lambda (x) (f (ff x))))))
  (iter n f))
;compose递归
(define (compose f g)
  (lambda (x)
    (f (g x))))
(define (repeated2 f n)
  (if (= n 1)
      f
      (compose f (repeated2 f (- n 1)))))
;compose迭代
(define (repeated3 f n)
  (define (iter i ff)
    (if (= i 1)
        ff
        (iter (- i 1) (compose f ff))))
  (iter n f))

(define (square x)
  (* x x))
((repeated3 square 2) 5)
