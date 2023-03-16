#lang racket

(define-struct leaf () #:transparent)
(define-struct node (l elem r) #:transparent)

(define (foldr f elem xs)
    (if (leaf? xs) 
        elem
        (foldr f)))
