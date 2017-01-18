#lang planet neil/sicp
;table
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
;deriv
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))
;package
(define (=number? x y)
  (and (number? x) (number? y) (= x y)))
(define (attach-tag type-tag x y)
  (list type-tag x y))
(define (type-tag datum)
  (car datum))
(define (contents datum)
  (cdr datum))
(define (install-sum-package)
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (make-sum x y)
    (cond ((=number? x 0) y)
          ((=number? y 0) x)
          ((and (number? x) (number? y))
           (+ x y))
          (else
           (attach-tag '+ x y))))
  (put 'addend '+ addend)
  (put 'augend '+ augend)
  (put 'make-sum '+ make-sum)
  (put 'deriv '+ (lambda (exp var)
                   (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var))))
  'done)
(install-sum-package)
(deriv '(+ x 3) 'x)
;product
(define (install-product-package)
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (make-product x y)
    (cond ((or (=number? x 0) (=number? y 0)) 0)
          ((=number? x 1) y)
          ((=number? y 1) x)
          ((and (number? x) (number? y)) (* x y))
          (else
           (attach-tag '* x y))))
  (define (make-sum x y)
    (cond ((=number? x 0)
           y)
          ((=number? y 0)
           x)
          ((and (number? x) (number? y))
           (+ x y))
          (else
           (attach-tag '+ x y))))
  (put 'multiplier '* multiplier)
  (put 'multiplicand '* multiplicand)
  (put 'make-product '* make-product)
  (put 'deriv '* (lambda (exp var)
                   (make-sum
                    (make-product (multiplier exp) (deriv (multiplicand exp) var))
                    (make-product (multiplicand exp) (deriv (multiplier exp) var)))))
  'done)
(install-product-package)
(deriv '(* x y) 'x)
;exponentiation
(define (install-exponentiation-package)
  (define (base s) (car s))
  (define (exponent s) (cadr s))
  (define (make-exponentiation x y)
    (cond ((=number? y 0) 1)
          ((=number? y 1) x)
          ((and (number? x) (number? y)) (expt x y))
          (else
           (attach-tag '** x y))))
  (define (make-product x y)
    (cond ((or (=number? x 0) (=number? y 0)) 0)
          ((=number? x 1) y)
          ((=number? y 1) x)
          ((and (number? x) (number? y)) (* x y))
          (else
           (attach-tag '* x y))))
  (put 'base '** base)
  (put 'exponent '** exponent)
  (put 'make-exponentiation '** 'make-exponentiaion)
  (put 'deriv '** (lambda (exp var)
                    (make-product (exponent exp)
                                  (make-product
                                   (make-exponentiation (base exp)
                                                        (- (exponent exp) 1))
                                   (deriv (base exp) var)))))
  'done)
(install-exponentiation-package)
(deriv '(** x 3) 'x)