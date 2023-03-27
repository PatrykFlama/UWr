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

(define (f5 a_ab a)
  (if (some? (a_ab a))
      (cons (snd (some-v (a_ab a)))
            (f5 a_ab (fst (some-v (a_ab a)))))
      '()))

; --- zad2 ---
(define (apply f x) (f x))
(define (compose f g) (lambda (x) (f (g x))))
(define (flip f) (lambda (x y) (f y x)))
(define (curry f) (lambda (x) (lambda (y) (f x y))))

; --- zad3 ---
; ( curry compose )
; (( curry compose ) ( curry compose ))
; (( curry compose ) ( curry apply ))
; (( curry apply ) ( curry compose ))
; ( compose curry flip )

; --- zad4 ---
(define (remove x list)
  (cond
    [(empty? list) '()] 
    [(equal? x (first list)) (rest list)]
    [else (cons (first list) (remove x (rest list)))]))

(define (perms inlist)
    (local [
        (define (_perms _inlist elements)   ; [original list for permutation] [elements to pop]
            (cond
                [(empty? elements) '()]
                [(empty? (rest _inlist)) (list _inlist)]
                [else (append
                        (map ; add deleted element to all subsets from list without that element 
                            (lambda (x) (cons (first elements) x))
                            (perms (remove (first elements) _inlist)))
                        (_perms _inlist (rest elements)))]))]   ; iteration over all elements
    (if (empty? inlist)
        (list '())
        (_perms inlist inlist))))
  
          


















              