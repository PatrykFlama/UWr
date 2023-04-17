#lang plait

; zad1
(define-type (2-3-tree 'a)
    (leaf)
    (2-node (l : (2-3-tree 'a)) (elem : 'a) (r : (2-3-tree 'a)))
    (3-node (l : (2-3-tree 'a)) (elem-l : 'a) (mid : (2-3-tree 'a)) (elem-r : 'a) (r : (2-3-tree 'a)))
    (4-node (l : (2-3-tree 'a)) (elem-l : 'a) (mid : (2-3-tree 'a)) (elem-r : 'a) (r : (2-3-tree 'a)))
        (to-evaluate : 'a)) ; helper type for 2nd exercise

(define example-tree
    (3-node
        (3-node (leaf) 2 (leaf) 3 (leaf))
        5
        (2-node (leaf) 6 (leaf))
        9
        (2-node (leaf) 10 (leaf))))

(define (3= a b c) (and (= a b) (= b c)))
(define (3< a b c) (and (< a b) (< b c)))
(define (4< a b c d) (and (< a b) (< b c) (< c d)))

(define (2-3-tree?? t)
    (local(
        (define (check_order minn maxx t)
            (type-case (2-3-tree 'a) t
                [(leaf) #t]
                [(2-node l elem r) 
                    (and
                        (3< minn elem maxx)
                        (check_order minn elem l)
                        (check_order elem maxx r))]
                [(3-node l e-l mid e-r r)
                    (and
                        (4< minn e-l e-r maxx)
                        (check_order minn e-l l)
                        (check_order e-l e-r mid)
                        (check_order e-r maxx r))]))

        (define false -inf.0)
        (define (t? n) (not (= n false)))
        (define (check_height t)    ; height has to be equal in all subtrees
            (type-case (2-3-tree 'a) t
                [(leaf) 0]
                [(2-node l elem r)
                    (let [(h_l (check_height l)) (h_r (check_height r))]
                    (if (and (t? h_l) (t? h_r) (= h_l h_r)) (+ h_l 1) false))]
                [(3-node l e-l mid e-r r)
                    (let [(h_l (check_height l)) (h_m (check_height mid)) (h_r (check_height r))]
                    (if (and (t? h_l) (t? h_m) (t? h_r) (3= h_l h_m h_r)) (+ h_l 1) false))])))
    (and 
        (check_order -inf.0 +inf.0 t)
        (t? (check_height t)))))
    ; (pair 
    ;     (check_order -inf.0 +inf.0 t)
    ;     (t? (check_height t)))))

; zad2
(define BUILDRES #f)
(define (insert el t)
    (define (_insert el t res)
        (cond 
            [(leaf? t) (2-node (leaf) el (leaf))]   ; if tree is empty
            [(2-node? t) (cond
                [(leaf? (2-node-l t))
                    t]   ;TODO insert here
                [(< el (2-node-elem t))
                    (_insert el (2-node-l t) BUILDRES)]
                [(< (2-node-elem t) el)
                    (_insert el (2-node-r t) BUILDRES)])]
            [(3-node? t) (cond
                [(leaf? (3-node-l t))
                    ] ;TODO insert here
                [(< el (3-node-elem-l t))
                    (_insert el (3-node-l t) BUILDRES)]
                [(< (3-node-elem-r t) el)
                    (_insert el (3-node-r t) BUILDRES)]
                [else
                    (_insert el (3-node-mid t) BUILDRES)])]
            )))
