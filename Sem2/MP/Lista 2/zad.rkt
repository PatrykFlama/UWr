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
(define (select xs)
  (define (search left_s right_s min)   ; left - visited part of list, without min element
    (cond [(null? right_s) (cons min left_s)]
          [(< (car right_s) min) (search (cons min left_s) (cdr right_s) (car right_s))]
          [else (search (cons (car right_s) left_s) (cdr right_s) min)]))
  (if (null? xs) xs
      (search '() (cdr xs) (car xs))))

(define (select-sort xs)
  (define (_select-sort to_sort sorted)
    (cond [(null? to_sort) sorted]
          [else (let ([min (select to_sort)])
                      (_select-sort (cdr min) (append sorted (list (car min)))))]))
  (_select-sort xs '()))

(check-equal? (select-sort (list 87 9 7 5 6 4 0 2 3 1 5)) '(0 1 2 3 4 5 5 6 7 9 87))
(check-equal? (select-sort ( list 1 5 0 7 1 4 1 0)) '(0 0 1 1 1 4 5 7))

; zad 10
(define (len lst)
  (cond [(null? lst) 0]
        [else (+ 1 (length (rest lst)))]))

(define (app-elem lst elem)
  (append lst (list elem)))

(define (split xs)
  (define (_split left lst len)
    (cond [(= len 0) (cons left lst)]
          [else (_split (append left (list (car lst))) (cdr lst) (- len 1))]))
  (_split '() xs (floor (/ (len xs) 2))))

#|
(define (merge left right)
  (define (_merge left right res)
    (cond [(and (null? left) (null? right)) res]
          [(null? left) (_merge left (cdr right) (app-elem res (car right)))]
          [(null? right) (_merge (cdr left) right (app-elem res (car left)))]
          [(< (car left) (car right)) (_merge (cdr left) right (app-elem res (car left)))]
          [else (_merge left (cdr right) (app-elem res (car right)))]))
  (_merge left right '()))
|#
(define (merge left right)
  (cond [(empty? left) right]
        [(empty? right) left]
        [(<= (car left) (car right))
         (append (list (car left)) (merge (cdr left) right))]
        [else
         (append (list (car right)) (merge left (cdr right)))]))


(define (merge-sort lst)
  (cond [(= (len lst) 1) lst]
        [else (let ([split (split lst)])
                (merge (merge-sort (car split)) (merge-sort (cdr split))))]))

(check-equal? (merge-sort (list 2 4 6 8 9 7 5 1 0)) '(0 1 2 4 5 6 7 8 9))

; merge-sort jest strukturalnie rekurencyjny
















































