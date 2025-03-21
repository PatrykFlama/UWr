#lang plait

; --- zad1 ---
; (' a 'b - > 'a)
(define (f1 a b) a)
; ((' a 'b - > 'c) (' a -> 'b) 'a -> 'c)
(define (f2 c b a) (c a (b a)))
; (((' a - > 'a) -> 'a) -> 'a)
(define (f3 [a : (('a -> 'a) -> 'a)]) (a identity))
(define (f3v2 a) (a (lambda (x) (a (lambda (x) x)))))
; ((' a - > 'b) (' a -> 'c) -> ('a -> (' b * 'c)))
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
; (curry compose)
; ((curry compose) (curry compose))
; ((curry compose) (curry apply))
; ((curry apply) (curry compose))
; (compose curry flip)

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

; --- zad6 ---
(define-type (RoseTree 'a)
    (rose-leaf [elem : 'a])
    (rose-node [sub : (Listof (RoseTree 'a))]))

(define example-rose-tree
    (rose-node (list
        (rose-node (list 
            (rose-leaf 1)
            (rose-leaf 2)))
        (rose-node (list
            (rose-leaf 3)
            (rose-node (list
                (rose-leaf 4)))))
        (rose-leaf 5)
    (rose-leaf 6))))

(define (dfs tree)
    (local[
        (define (_dfs queue)
            (if (empty? queue)
                '()
                (append (dfs (first queue)) (_dfs (rest queue)))))] 
    (if (rose-leaf? tree) 
        (list (rose-leaf-elem tree))
        (_dfs (rose-node-sub tree)))))

; --- zad7 ---
(define-type Prop
    (var [v : String])
    (conj [l : Prop] [r : Prop])
    (disj [l : Prop] [r : Prop])
    (neg [f : Prop]))

(define example-prop
    (conj 
        (disj
            (neg (var "a"))
            (var "b"))
        (conj
            (var "b")
            (neg (var "c")))))

(define (free-vars [f : Prop])
    (local
        [(define (add-unique x xs)
            (cond
                [(empty? xs) (list x)]
                [(equal? x (first xs)) xs]
                [else (cons
                    (first xs)
                    (add-unique x (rest xs)))]))
         (define (merge-unique xs ys)
            (if (empty? (rest xs)) 
                (add-unique (first xs) ys)
                (add-unique (first xs) (merge-unique (rest xs) ys))))]
        (cond 
            [(var? f) (list (var-v f))]
            [(conj? f) (merge-unique 
                    (free-vars (conj-l f))
                    (free-vars (conj-r f)))]
            [(disj? f) (merge-unique 
                    (free-vars (disj-l f))
                    (free-vars (disj-r f)))]
            [(neg? f) (free-vars (neg-f f))])))

#|
hash : ((Listof (' a * 'b)) -> (Hashof 'a 'b))
hash-ref : ((Hashof 'a 'b) 'a -> (Optionof 'b))
hash-set : ((Hashof 'a 'b) 'a 'b -> (Hashof 'a 'b))

eval : ((Hashof String Boolean) Prop -> Boolean)
|#

(define (eval [h : (Hashof String Boolean)] [p : Prop])
    (cond
        [(var? p) 
            (some-v (hash-ref h (var-v p)))]
        [(conj? p) 
            (and    
                (eval h (conj-l p))
                (eval h (conj-r p)))]
        [(disj? p)
            (or
                (eval h (disj-l p))
                (eval h (disj-r p)))]
        [(neg? p) 
            (not (eval h (neg-f p)))]))

(define example-hash (hash (list
    (pair "a" #f)
    (pair "b" #t)
    (pair "c" #f))))

; --- zad9 ---
(define (tautology [p : Prop])
    (local[
        (define (_taut h fv)
            (if (empty? fv)
                (eval h p)
                (and 
                    (_taut 
                        (hash-set h (first fv) #t)
                        (rest fv))
                    (_taut 
                        (hash-set h (first fv) #f)
                        (rest fv)))))]
    (_taut (hash '()) (free-vars p))))

(define example-tautology-easy
  (disj
   (neg (var "p"))
   (var "p")))

(define example-tautology-hard
    (disj
        (neg
            (conj
            (conj
                (disj
                    (neg (var "p"))
                    (var "q"))
                (disj
                    (neg (var "q"))
                    (var "r")))
                (disj
                    (var "p")
                    (var "q"))))
        (var "r")))













