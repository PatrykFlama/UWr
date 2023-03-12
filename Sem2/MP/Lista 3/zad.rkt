#lang racket

; Zadanie 1
'((car (a . b)) (* 2))
(list `(car (a . b)) `(* 2))
`(,( car '(a . b)) ,(* 2))
(list (car '(a . b)) (* 2))
'((+ 1 2 3) ( cons ) ( cons a b))
(list '(+ 1 2 3) '(cons) '(cons a b))

; Zadanie 2
(define (mult x acc) (* x acc))

(define (mult-list xs)
    (foldl mult 1 xs))

; Zadanie 4
(define (my-compose f g)
    (lambda (x) (f (g x))))

(( my-compose square inc ) 5)
(( my-compose inc square ) 5)

; Zadanie 5
(define (negatives n)
    (build-list n (lambda (x) (- (* x -1) 1))))

(define (reciprocals n)
    (build-list n (lambda (x) (/ 1 (+ x 1)))))

(define (evens n)
    (build-list n (lambda (x) (* x 2))))

(define (identityM n)
    (build-list n
        (lambda (col) (build-list n
            (lambda (row)
                (if (= col row) 1 0))))))


; Zadanie 6
(define empty-set '())


; Zadanie 7
(define ( foldr-reverse xs )
    (foldr ( lambda (y ys ) ( append ys ( list y))) null xs ))

(length (foldr-reverse (build-list 10000 identity)))