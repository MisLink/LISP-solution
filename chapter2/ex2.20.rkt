#lang racket
(define (same-party first . l)
  (filter (if (even? first)
              even?
              odd?)
          (cons first l)))

(same-party 2 3 4 5 6)