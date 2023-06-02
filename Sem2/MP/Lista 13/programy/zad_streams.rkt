#lang racket

;; processing data sequences with lists --------------------

(define (second-prime-in-interval a b)
  (car (cdr (filter prime?
                    (enumerate-interval a b)))))

; (second-prime-in-interval 10000 5000000)

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define (square x)
  (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; streams aka lazy lists ---------------------------------
;* so streams are essentially lists with delayed cdr evaluation

;; delay and force
;* delay - promises to calculate the value of function, but not immediately
;* force - forces the calculation of a delayed function

(define-syntax-rule
  (stream-cons v s)
  (cons v (delay s)))

(define stream-car car)

(define (stream-cdr s)
  (force (cdr s)))

(define stream-null null)
(define stream-null? null?)

;; operations on streams

(define (stream-ref s n)    ; return nth element of stream s
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-filter p s)     ; erase elements of s that don't satisfy p, where p is a predicate
  (if (stream-null? s)
      stream-null
      (if (p (stream-car s))
          (stream-cons (stream-car s)
                       (stream-filter p (stream-cdr s)))
          (stream-filter p (stream-cdr s)))))

(define (stream-enumerate-interval a b)
  (if (> a b)
      stream-null
      (stream-cons a (stream-enumerate-interval (+ a 1) b))))

(define (stream-second-prime-in-interval a b)
  (stream-car
   (stream-cdr
    (stream-filter prime?
                   (stream-enumerate-interval a b)))))

;; infinite streams

(define ones (stream-cons 1 ones))

(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))

(define nats (integers-from 0))

(define (sieve s)
  (stream-cons
   (stream-car s)
   (sieve
    (stream-filter
     (λ (x) (not (divides? (stream-car s) x)))
     (stream-cdr s)))))

(define primes (sieve (integers-from 2)))

; zad2
(define (check-if-prime n)
  (define (check-if-prime-helper n s)
    (cond
      [(> (square (stream-car s)) n) #t]
      [(divides? (stream-car s) n) #f]
      [else (check-if-prime-helper n (stream-cdr s))]))
  (check-if-prime-helper n primes2))
(define primes2
  (stream-cons
   2
   (stream-filter
    (λ (x) (check-if-prime x))
    (integers-from 3))))

 ;; combining (infinite) streams 

(define (map2 f xs ys)
  (stream-cons
   (f (stream-car xs)
      (stream-car ys))
   (map2 f (stream-cdr xs) (stream-cdr ys))))

(define nats2 (stream-cons 0 (map2 + nats2 ones)))

; generate
(define (stream-gen s n)
  (if (= n 0)
      stream-null
      (cons (stream-car s)
            (stream-gen (stream-cdr s) (- n 1)))))

#|
nats2    0 1 2 ...
ones     1 1 1 ...
---------------------------------------
nats2  0 1 2 3 ...
|#

(define fibs
  (stream-cons 0 (stream-cons 1 (map2 + fibs (stream-cdr fibs)))))

; zad3
(define fact
  (stream-cons 1 (map2 * (integers-from 1) fact)))

; zad4
(define (partial-sums s)
  (stream-cons
   (stream-car s)
   (map2 + (stream-cdr s) (partial-sums s))))

#|
fib            0 1 1 2 3 ...  
(cdr fib)      1 1 2 3 ...
-----------------------------------------
fib        0 1 1 2 3 5 ...

|#

;; our own implementation of delay and force (needs to be moved at the top of the file)

#|
(define (memo-proc proc)
  (let ([already-run? false]
        [result false])
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax-rule
  (delay c)
  (memo-proc (λ () c)))

(define (force t)
  (t))
|#

; zad5
; assume that s1 and s2 are sorted and not null
(define (merge s1 s2)
  (cond
      [(= (stream-car s1) (stream-car s2))
        (stream-cons
          (stream-car s1)
          (merge
            (stream-cdr s1)
            (stream-cdr s2)))]
      [(< (stream-car s1) (stream-car s2))
        (stream-cons  
          (stream-car s1)
          (merge
            (stream-cdr s1)
            s2))]
      [else
        (stream-cons
          (stream-car s2)
          (merge
            s1
            (stream-cdr s2)))]))

(define (scale s t)
  (stream-cons
    (* (stream-car s) t)
    (scale (stream-cdr s) t)))

(define _hamming
  (stream-cons
    1
    (merge
      (scale _hamming 2)
      (merge
        (scale _hamming 3)
        (scale _hamming 5)))))
(define hamming
  (stream-cdr _hamming))

; (stream-gen fact 10)
; (stream-gen hamming 10)
