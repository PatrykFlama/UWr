#lang plait

; --- zad1 ---
; ( ' a 'b - > 'a)
(define (f1 a b) a)
; (( ' a 'b - > 'c) ( ' a -> 'b) 'a -> 'c)
(define (f2 c b a) (c a (b a)))
; ((( ' a - > 'a) -> 'a) -> 'a)
(define (f3 [a : (('a -> 'a) -> 'a)]) (a identity))
; (( ' a - > 'b) (' a -> 'c) -> ( 'a -> ( ' b * 'c)))
(define (f4 [b : ('a -> 'b)] [c : ('a -> 'c)])
  (lambda (a) (pair (b a) (c a))))

(define (f5 aoab a)
  (if (some? (aoab a))
      (cons (snd (some-v (aoab a)))
            (f5 aoab (fst (some-v (aoab a)))))
      empty))


; --- zad4 ---
(define (remove x list)
  (cond
    [(empty? list) '()] 
    [(equal? x (first list)) (rest list)]
    [else (cons (first list) (remove x (rest list)))]))

(define (_perms _inlist elements)
  (cond
    [(empty? elements) '(666)]
    [(empty? _inlist) '(777)]
    [else (append
           (_perms _inlist (rest elements))
           (_perms (remove (first elements) _inlist) elements))]))


(define (perms list)
  (_perms list list))
  
          


















              