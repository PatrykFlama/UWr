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
                [(empty? elements) '()] ; empty elements queue
                [(empty? (rest _inlist)) (list _inlist)]    ; one element left
                [else (append
                        (map ; add deleted element to all subsets from list without that element 
                            (lambda (x) (cons (first elements) x))
                            (perms (remove (first elements) _inlist)))
                        (_perms _inlist (rest elements)))]))]   ; iteration over all elements
    (if (empty? inlist)
        (list '())
        (_perms inlist inlist))))
  
; --- zad5 ---
(define-type (Tree 'a)
    (leaf)
    (node [l : (Tree 'a)] [elem : 'a] [r : (Tree 'a)]))

(define example-tree
    (node (node (leaf) 1 (leaf))
          2
          (node (leaf)
                3
                (node (leaf) 4 (leaf)))))

#|
    f_node      acc res_left node_val res_right
    f_leaf      acc
    f_acc_left  acc node_val
    f_acc_right acc node_val
    acc/res - from root to node
    tree
|#
(define (process-tree f_node f_leaf f_acc_left f_acc_right acc tree)
    (if (leaf? tree)
        (f_leaf acc)
        (f_node 
            acc
            (process-tree f_node f_leaf f_acc_left f_acc_right
                (f_acc_left acc (node-elem tree))
                (node-l tree))
            (node-elem tree)
            (process-tree f_node f_leaf f_acc_left f_acc_right
                (f_acc_right acc (node-elem tree))
                (node-r tree)))))

(define (bst? tree)
    (process-tree   ; f_node f_leaf f_acc_left f_acc_right acc tree
        (lambda (acc res_left elem res_right)
            (and res_left res_right
                (< (fst acc) elem) (> (snd acc) elem)))
        (lambda (acc) #t)
        (lambda (acc elem)
            (pair (fst acc) elem))
        (lambda (acc elem)
            (pair elem (snd acc)))
        (pair -inf.0 +inf.0) tree))

(define (sum-paths tree)
    (process-tree   ; f_node f_leaf f_acc_left f_acc_right acc tree
        (lambda (acc res_left elem res_right)
            (node res_left (+ acc elem) res_right))
        (lambda (acc) (leaf))
        (lambda (acc elem)
            (+ acc elem))
        (lambda (acc elem)
            (+ acc elem))
        0 tree))























