#lang racket

(define (make-counter)
  (define cnt 0)
  (lambda ()
    (set! cnt (+ cnt 1))
    cnt))

(define c1 (make-counter))
(define c2 (make-counter))

(define (foldl-map f s xs)
  (let ([ys (map (lambda (x)
                   (let ([p (f s x)])
                     (set! s (car p))
                     [cdr p]))
                  xs)])
    {cons s ys}))
