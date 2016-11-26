;递归
(define (cont-frac n d k)
  (define (cf i)
    (if (= k i)
        (/ (n k) (d k))
        (/ (n i) (+ (d i) (cf (+ i 1))))))
  (cf 1))
;迭代
(define (cont-frac1 n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i) (+ (d i) result)))))
  (iter (- k 1) (/ (n k) (d k))))

(define (gold-ratio k)
  (+ 1
     (cont-frac1 (lambda (i) 1.0)
                (lambda (i) 1.0)
                k)))
(gold-ratio 12)