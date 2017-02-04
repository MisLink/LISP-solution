#lang planet neil/sicp
;信号
(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures))
          'done))
    (define (add-action! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-signal!)
            ((eq? m 'add-action!) add-action!)
            (else
             (error "unknown operation."))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire x)
  (wire 'set-signal!) x)
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))
;队列
(define (make-queue)
  (cons '() '()))
(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (set-front-ptr! q item) (set-car! q item))
(define (set-rear-ptr! q item) (set-cdr! q item))
(define (empty-queue? q)
  (null? (front-ptr q)))
(define (front-queue q)
  (if (empty-queue? q)
      (error "empty queue")
      (car (front-ptr q))))
(define (insert-queue! q item)
  (let ((new (cons item '())))
    (if (empty-queue? q)
        (begin
          (set-front-ptr! q new)
          (set-rear-ptr! q new)
          (print-queue q))
        (begin
          (set-cdr! (rear-ptr q) new)
          (set-rear-ptr! q new)
          (print-queue q)))))
(define (delete-queue! q)
  (if (empty-queue? q)
      (error "empty queue")
      (begin
        (set-front-ptr! q (cdr (front-ptr q)))
        (print-queue q))))
(define (print-queue q)
  (car q))
;处理表
(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s)
  (car s))

(define (segment-queue s)
  (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda)
  (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda)
  (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda)
  (car (segments agenda)))

(define (rest-segment agenda)
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments)) action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments (cons (make-new-time-segment time action)
                                       (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda (cons (make-new-time-segment time action) segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segment agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "agenda is empty")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))
;;;
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " new value: ")
                 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)


(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))
(define (or-gate a1 a2 output)
  (define (and-action-procdure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procdure)
  (add-action! a1 and-action-procdure))
(define (logical-or x y)
  (if (or (= x 1) (= y 1))
      1
      0))
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input))

(define (logical-not x)
  (cond ((= x 1) 0)
        ((= x 0) 1)
        (else
         (error "error signal"))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a1 and-action-procedure))
(define (logical-and x y)
  (cond ((or (= x 0) (= y 0)) 0)
        (else
         1)))

(define input (make-wire))
(define output (make-wire))
(inverter input output)
the-agenda
(set-signal! input 0)
(propagate)
(get-signal output)
the-agenda
