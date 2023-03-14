#lang racket

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
; czy empty set zawiera element x
(define empty-set
  (lambda (x) #f))

; czy lista składająca się z elementu a zawiera element x
(define (singleton a)
  (lambda (x) (equal? x a)))

; s jest zbiorem, a elementem
(define (in a s) (s a))

(define (union s t)
  (lambda (x) (or (s x) (t x))))

(define (intersect s t)
  (lambda (x) (and (s x) (t x))))


; Zadanie 7
(define ( foldr-reverse xs )
    (foldr ( lambda (y ys) ( append ys ( list y))) null xs ))
; 
(length (foldr-reverse (build-list 10000 identity)))

; Zadanie 8
; llist jest listą reprezentowaną przez funkcję
; funckja ta pamięta jedną listę oraz po zaaplikowaniu do innej listy zwraca ich konkatencję
(define (list->llist xs)
  (lambda (x) (append xs x)))

(define (llist->list f) (f null))

(define llist-null (list->llist '()))

(define (llist-singleton x)
  (list->llist '(x)))

(define (llist-append f g)
  (lambda (x) (f (g x))))

(define (foldr-llist-reverse xs)
    (llist->list
        (foldr
            (lambda (y ys) (llist-append ys (list->llist '(y))))
            llist-null
            xs)))

(length (foldr-reverse (build-list 40000 identity)))
(length (foldr-llist-reverse (build-list 40000 identity)))
