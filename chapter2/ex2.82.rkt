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
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))
;test
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum)) 
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum)) 
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (apply-generic op . args)
  (define (coerce-list-to-type lst type)
    (if (null? lst)
        '()
        (let ((t1->t2 (get-coercion (type-tag (car lst)) type)))
          (if t1->t2
              (cons (t1->t2 (car lst)) (coerce-list-to-type (cdr lst)))
              (cons (car lst) (coerce-list-to-type (cdr lst) type))))))
  (define (apply-coerced lst)
    (if (null? lst)
        (error "No method for given arguments")
        (let ((coerced-list (coerce-list-to-type args (type-tag (car lst)))))
          (let ((proc (get op (map type-tag coerced-list))))
            (if proc
                (apply proc (map contents coerced-list))
                (apply-coerced (cdr lst)))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (apply-coerced args)))))
