#lang racket

; zad3
(define (suffixes xs)
    (cond 
        [(empty? xs) (list '())]
        [else (cons '() (map (lambda (ys) (cons (first xs) ys)) (suffixes (rest xs))))]))

(define/contract (suffixes-contracted xs)
    (parametric->/c [a] (-> (listof a) (listof (listof a))))
    (cond 
        [(empty? xs) (list '())]
        [else (cons '() (map (lambda (ys) (cons (first xs) ys)) (suffixes-contracted (rest xs))))]))

(define start (current-inexact-milliseconds))
(define t1 (suffixes (range 2000)))
(define stop (current-inexact-milliseconds))
(- stop start)
(define start-c (current-inexact-milliseconds))
(define t2 (suffixes-contracted (range 2000)))
(define stop-c (current-inexact-milliseconds))
(- stop-c start-c)

; zad4
(define/contract (f1 a b)
    (parametric->/c [a b] (-> a b a))
    a)
(f1 1 "b")

(define/contract (f2 fabc fab a)
    (parametric->/c [a b c] (-> (-> a b c) (-> a b) a c))
    (fabc a (fab a)))
(f2 + identity 2)

(define/contract (f3 fbc fab)
    (parametric->/c [a b c] (-> (-> b c) (-> a b) (-> a c)))
    (lambda (a) ((fab a) fbc)))


(define/contract (f4 ffaa->a)
    (parametric->/c [a] (-> (-> (-> a a) a) a))
    (ffaa->a identity))

; zad5
(define/contract (foldl-map f a xs)
    (parametric->/c [x y acc] 
        (-> (-> x acc (cons/c y acc)) acc (listof x) 
        (cons/c (listof y) acc)))
    (define (it a xs ys)
        (if (null? xs)
            (cons (reverse ys) a)
            (let [(p (f (car xs) a))]
                (it (cdr p)
                    (cdr xs)
                    (cons (car p) ys)))))
    (it a xs null))

(foldl-map (lambda (x a) (cons a (+ a x))) 0 '(1 2 3))

; zad6
(define/contract (fold-right f acc xs)
    (parametric->/c [a b] (-> (-> a b b) b (listof a) b))
    (if (empty? xs) acc
        (f
            (fold-right f acc (rest xs))    ; this line
            (first xs))))                   ; is switched with this

(define/contract (fold-right-mod f acc xs)
    (parametric->/c [a] (-> (-> a a a) a (listof a) a))
    (if (empty? xs) acc
        (f
            (fold-right-mod f acc (rest xs))
            (first xs))))


(fold-right-mod     ; here proabably its smth like optionof number and bool
    (lambda (el acc)
        (if (equal? el 2) acc #f))
    #t '(2 2 2 2))
#;(fold-right
    (lambda (el acc)
        (if (equal? el 2) acc #f))
    #t '(2 2 2 2))

