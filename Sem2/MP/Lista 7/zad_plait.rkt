#lang plait

; zad1
(define-type (2-3-tree 'a)
    (leaf)
    (2-node (l : (2-3-tree 'a)) (elem : 'a) (r : (2-3-tree 'a)))
    (3-node (l : (2-3-tree 'a)) (elem-l : 'a) (mid : (2-3-tree 'a)) (elem-r : 'a) (r : (2-3-tree 'a))))
    ; TODO e-r < e-l

; TODO example tree (so learn about 2-3 trees)

(define (2-3-tree? t)
    (local(
        (define (check_order minn maxx t)
            (type-case (2-3-tree 'a) t
                [(leaf) #t]
                [(2-node l elem r) 
                    (and
                        (< minn elem maxx)
                        (check_order minn elem l)
                        (check_order elem maxx r))]
                [(3-node l e-l mid e-r r)
                    (and
                        (< minn e-r e-l maxx)
                        (check_order minn e-r l)
                        (check_order e-r e-l mid)
                        (check_order e-l maxx r))]))

        (define false -inf.0)
        (define (t? n) (not (= n false)))
        (define (check_height t)    ; height has to be equal in all subtrees
            (type-case (2-3-tree 'a) t
                [(leaf) 0]
                [(2-node l elem r)
                    (let [(h_l (check_height l)) (h_r (check_height r))]
                    (if (and (t? h_l) (t? h_r) (= h_l h_r)) (+ h_l 1) false))]
                [(3-node l e-l mid e-r r)
                    (let [(h_l (check_height l) (h_m (check_height mid)) (h_r (check_height r)))]
                    (if (and (t? h_l) (t? h_m) (t? h_r) (= h_l h_m h_r)) (+ h_l 1) false))])))
    (and 
        (check_order -inf.0 +inf.0 t)
        (check_height t))))

