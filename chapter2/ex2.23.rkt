#lang racket
(define (for-each func lst)
  (cond ((not (null? lst))
         (func (car lst))
         (for-each func (cdr lst)))))
(for-each (lambda (x) (newline) (display x)) (list 57 321 88))