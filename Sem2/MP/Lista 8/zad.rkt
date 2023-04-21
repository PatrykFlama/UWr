#lang racket

(define (mlist xs)
    (if (null? xs) '() 
        (mcons 
            (first xs)
            (mlist (rest xs)))))

; zad1
(define (cycle! xs)
    (define (_cycle! ys)
        (if (null? (mcdr ys))
            (set-mcdr! ys xs)
            (_cycle! (mcdr ys))))
    (if (null? xs) '()
        (_cycle! xs))
    xs)

; zad2
(define (mreverse! xs)
    (define (_mreverse! xs prev)
        (if (null? xs) prev
            (begin
                (_mreverse! (mcdr xs) xs)
                (set-mcdr! xs prev)
                )))
    (_mreverse! xs '())
    xs)
