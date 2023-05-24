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
            (begin (let
                [(tcdr (mcdr xs))]
                (set-mcdr! xs prev)
                (_mreverse! tcdr xs)))))
    (_mreverse! xs '()))


; ----TESTS-----
(cycle! (mlist '(1 2 3 4)))
(mreverse! (mlist '(1 2 3 4)))

