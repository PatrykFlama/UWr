#lang plait

(define (my-map f xs)
  (type-case (Listof 'a) xs
    [(cons x xs) (cons (f x) (my-map f xs))]
    [empty       empty]))

(define-type (Tree 'a)
  (leaf)
  (node [l : (Tree 'a)] [elem : 'a] [r : (Tree 'a)]))

(define (insert x t)
  (type-case (Tree 'a) t
    [(leaf) (node (leaf) x (leaf))]
    [(node l y r)
     (if (< x y)
         (node (insert x l) y r)
         (node l y (insert x r)))]))

; ===========================================================

; Lista (cons h f) jest pusta wtw cała kolejka jest pusta
(define-type (Queue 'a)
  (empty-queue)
  (queue (h : 'a) (f : (Listof 'a)) (r : (Listof 'a))))

(define (mk-queue f r)
  (type-case (Listof 'a) f
    [empty
      (type-case (Listof 'a) (reverse r)
        [empty (empty-queue)]
        [(cons h f) (queue h f empty)])]
    [(cons h f) (queue h f r)]))

(define queue-empty
  (empty-queue))

(define (push q x)
  (type-case (Queue 'a) q
    [(empty-queue) (queue x empty empty)]
    [(queue h f r) (queue h f (cons x r))]))

(define (is-empty? q)
  (type-case (Queue 'a) q
    [(empty-queue) #t]
    [(queue h f r) #f]))

(define (peek q)
  (type-case (Queue 'a) q
    [(empty-queue) (none)]
    [(queue h f r) (some h)]))

(define (pop q)
  (type-case (Queue 'a) q
    [(empty-queue) (error 'pop "Queue is empty")]
    [(queue h f r) (mk-queue f r)]))