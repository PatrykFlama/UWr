#lang racket
(require rackunit)


; zad 2
(define (fib-iter n)
  (define (_fib n laster last)
    (if (= n 0) laster
        (_fib (- n 1) last (+ laster last))))
  (_fib n 0 1))

(define (fib-rek n)
  (cond ([= n 0] 0)
        ([= n 1] 1)
        (else [+ (fib-rek (- n 1)) (fib-rek (- n 2))])))

; zad 3
(define-struct matrix (a b c d) #:transparent)

(define (matrix-mult m n)
  (matrix
    (+ (* (matrix-a m) (matrix-a n)) (* (matrix-b m) (matrix-c n)))
    (+ (* (matrix-a m) (matrix-b n)) (* (matrix-b m) (matrix-d n)))
    (+ (* (matrix-c m) (matrix-a n)) (* (matrix-d m) (matrix-c n)))
    (+ (* (matrix-c m) (matrix-b n)) (* (matrix-d m) (matrix-d n)))))

(define matrix-id (matrix 1 0 0 1))

(define (matrix-expt m k)
  (cond ([= k 0] matrix-id)
        ([= k 1] m)
        (else [matrix-mult m (matrix-expt m (- k 1))])))

(define (fib-matrix n)
  (define start-matrix (matrix 1 1 1 0))
  (define res-matrix (matrix-expt start-matrix n))
  (matrix-b res-matrix))

; zad 4
(define (matrix-expt-fast m k)
  (cond ([= k 0] matrix-id)
        ([= k 1] m)
        ([= (modulo k 2) 0]
          [let ([half (matrix-expt-fast m (floor (/ k 2)))]) [matrix-mult half half]])
        (else [matrix-mult (matrix-expt-fast m (- k 1)) m])))

(define (fib-fast n)
  (define start-matrix (matrix 1 1 1 0))
  (define res-matrix (matrix-expt-fast start-matrix n))
  (matrix-b res-matrix))

; zad 5
(define (elem? x xs)
  (cond ([equal? xs '()] #f)
        ([equal? (car xs) x] #t)
        (else [elem? x (cdr xs)])))

(check-equal? (elem? 8 (list 1 6 2 8 3 7 2 0 -1)) #t)
(check-equal? (elem? 8 (list 1 6 2 0 3 7 2 0 -1)) #f)

; zad 6
(define (maximum xs)
  (define (_maximum xs maxx)
    (cond ([equal? xs '()] -inf.0)
          (else [max (first xs) maxx (maximum (cdr xs))])))
  (_maximum xs -inf.0))

(check-equal? (maximum (list 1 2 3 4 -5 0 123 -1823)) 123.0)

; zad 7
(define (suffixes xs)
  (define (_suffixes xs suff)
    (cond ([equal? xs '()] suff)
          (else [_suffixes (cdr xs) (append suff (list (cdr xs)))])))
  (_suffixes xs (list xs)))

(check-equal? (suffixes (list 1 2 3)) '((1 2 3) (2 3) (3) ()))

; zad 8
(define (sorted? xs)
  (define (_sorted? xs previous)
    (cond ([equal? xs '()] #t)
          ([> previous (first xs)] #f)
          (else (_sorted? (cdr xs) (first xs)))))
  (_sorted? (cdr xs) (first xs)))

(check-equal? (sorted? (list 2 5 1 3 6 4 8 7 9 0)) #f)
(check-equal? (sorted? (list 1 2 3 4 5 6 7 8 9)) #t)

; zad 9

