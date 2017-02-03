#lang planet neil/sicp
(define (make-table)
  (list "*table*"))
(define (lookup key1 key2 table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (let ((record (assoc (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))
(define (insert! key1 key2 value table)
  (let ((subtable (assoc key1 (cdr table))))
    (if subtable
        (let ((record (assoc key2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable (cons (key2 value) (cdr subtable)))))
        (set-cdr! table
                  (cons (list key1 (cons key2 value))
                        (cdr table))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)))))))
