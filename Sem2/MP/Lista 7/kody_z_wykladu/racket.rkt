#lang racket

(define (assert name b)
  (if b #t
      (error name "Assertion failed")))

#;(define (fact n)
  (assert 'fact (natural? n))
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define/contract x
  number?
  42)

(define/contract (fact n)
  (-> natural? (and/c natural? positive?))
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define/contract (my-map f xs)
  (parametric->/c [a b] (-> (-> a b) (listof a) (listof b)))
  (match xs
    ['()         null]
    [(cons x xs) (cons (f x) (my-map f xs))]))

(define/contract (foo x)
  (parametric->/c [a] (-> a a))
  x)

(struct leaf () #:transparent)
(struct node (l elem r) #:transparent)

(define (insert x t)
  (match t
    [(leaf) (leaf)]
    [(node l y r)
     (if (< x y)
         (node (insert x l) y r)
         (node l y (insert x r)))]))