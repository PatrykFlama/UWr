#lang racket

; zad
(define (suffixes xs)
    (cond 
        [(empty? xs) (list '())]
        [else (cons '() (map (lambda (ys) (cons (first xs) ys)) (suffixes (rest xs))))]))


