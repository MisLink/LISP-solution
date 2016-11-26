#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (enumerate low high)
  (if (> low high)
      '()
      (cons low (enumerate (+ 1 low) high))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? k positions)
  (iter-check (car positions) (cdr positions) 1))

(define (iter-check new-queen rset-of-queens i)
  (if (null? rset-of-queens)
      true
      (let ((current-queen (car rset-of-queens)))
        (if (or (= new-queen current-queen)
                (= new-queen (+ i current-queen))
                (= new-queen (- current-queen i)))
            false
            (iter-check new-queen (cdr rset-of-queens) (+ i 1))))))

(define (queens board-size)
  (define (queens-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rset-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rset-of-queens))
                 (enumerate 1 board-size)))
          (queens-cols (- k 1))))))
  (queens-cols board-size))

(queens 4)
