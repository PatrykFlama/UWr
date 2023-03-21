#lang racket
(require rackunit)

; --- tree ---
(define-struct leaf () #:transparent)
(define-struct node (l elem r) #:transparent)

(define test-tree
    (node
        (node (leaf) 1 (leaf))
        2
        (node 
            (node (leaf) 3 (leaf))
            4
            (node (leaf) 5 (leaf)))))


; --- zad1 ---
#|
zamienia całą ścieżkę prowadzącą do wierzchołka w którym będzie wstawiać cyfrę

(define (find-bst x t)
  (cond [(leaf? t) #f]
        [(node? t)
         (cond [(= x (node-elem t)) #t]
               [(< x (node-elem t))
                (find-bst x (node-l t))]
               [else
                (find-bst x (node-r t))])]))

(define (insert-bst(car xs) (helper _tree (cdr xs)) x t)
  (cond [(leaf? t) (node (leaf) x (leaf))]
        [(node? t)
         (cond [(= x (node-elem t)) t]
                [(< x (node-elem t))
                 (node (insert-bst(car xs) (helper _tree (cdr xs)) x (node-l t))
                       (node-elem t)
                       (node-r t))]
                [else
                 (node (node-l t)
                       (node-elem t)
                       (insert-bst(car xs) (helper _tree (cdr xs)) x (node-r t)))])]))
|#

; --- zad2 ---
; (f res-left elem res-right)
(define (fold-tree f elem tree)
    (cond   [(leaf? tree) elem]
            [(node? tree) (f 
                (fold-tree f elem (node-l tree))
                (node-elem tree) 
                (fold-tree f elem (node-r tree)))]))

(define (tree-sum tree)
    (fold-tree + 0 tree))

(define (tree-flip tree)
    (fold-tree (lambda (l val r) (node r val l)) (leaf) tree))

(define (tree-height tree)
    (fold-tree (lambda (l val r) (+ (max l r) 1 )) 0 tree))

(define (tree-span tree)
    (cons   (fold-tree (lambda (l val r) (cond
                                                [(not (leaf? l)) l]
                                                [(not (leaf? r)) r]
                                                [else val]))
                        (leaf) tree)
            (fold-tree (lambda (l val r) (cond 
                                                [(not (leaf? r)) r]
                                                [(not (leaf? l)) l]
                                                [else val]))
                        (leaf) tree)))

(define (tree-span-bst tree)
    (cons   (fold-tree (lambda (l val r) (min l val r)) +inf.0 tree)
            (fold-tree (lambda (l val r) (max l val r)) -inf.0 tree)))

(define (flatten tree)
    (fold-tree (lambda (l val r) (append l (cons val r))) '() tree))

(check-equal? (tree-sum test-tree) 15)
(check-equal? (tree-flip test-tree) (node (node (node (leaf) 5 (leaf)) 4 
                                    (node (leaf) 3 (leaf))) 2 (node (leaf) 1 (leaf))))
(check-equal? (tree-height test-tree) 3)
(check-equal? (tree-span-bst test-tree) '(1.0 . 5.0))
(check-equal? (tree-span test-tree) '(1 . 5))
(check-equal? (flatten test-tree) '(1 2 3 4 5))


; --- zad3 ---
(define (bst? tree)
    (cond [(leaf? tree) #t]
          [(node? tree) (and 
            [or (leaf? (node-l tree)) 
                (< (node-elem (node-l tree)) (node-elem tree))]
            [or (leaf? (node-r tree)) 
                (> (node-elem (node-r tree)) (node-elem tree))]
            (bst? (node-l tree)) (bst? (node-r tree)))]))

(check-equal? (bst? test-tree) #t)

(define (sum-subtree tree)
    (cond   [(leaf? tree) (leaf)]
            [(node? tree) 
                (let [(l (sum-path (node-l tree))) (r (sum-path (node-r tree)))]
                    (node l (+
                            (if (leaf? l) 0 (node-elem l))
                            (if (leaf? r) 0 (node-elem r))
                            (node-elem tree))
                        r))]))

(define (sum-path tree)
    (define (_sum-path tree acc)
        (cond   [(leaf? tree) (leaf)]
                [(node? tree)
                    (let ((new-cost (+ acc (node-elem tree)))) (node 
                        (_sum-path (node-l tree) new-cost)
                        new-cost
                        (_sum-path (node-r tree) new-cost)))]))
    (_sum-path tree 0))


; --- zad4 ---
(define (list->left-tree xs)
    (foldl (lambda (x tree) (node tree x ( leaf))) (leaf) xs))
(define test-left-tree (list->left-tree(build-list 20000 identity)))

(define (flat-append tree xs)
    (cond [(leaf? tree) xs]
          [(node? tree)
            (flat-append (node-l tree)
                (cons (node-elem tree)
                    (flat-append (node-r tree) xs)))]))

(define (flatten-quick tree)
    (flat-append tree null))

(check-equal? (flat-append test-tree (list 10 11)) '(1 2 3 4 5 10 11))
(check-equal? (flatten-quick test-tree) '(1 2 3 4 5))

; --- zad5 ---
(define (insert-bst(car xs) (helper _tree (cdr xs)) x tree)
  (cond [(leaf? tree) (node (leaf) x (leaf))]
        [(node? tree)
         (cond  [(<= x (node-elem tree))
                    (node (insert-bst(car xs) (helper _tree (cdr xs)) x (node-l tree))
                          (node-elem tree)
                          (node-r tree))]
                [else
                    (node (node-l tree)
                          (node-elem tree)
                          (insert-bst(car xs) (helper _tree (cdr xs)) x (node-r tree)))])]))

(define (treesort xs)
    (define (_treesort xs bst)
        (cond   [(null? xs) bst]
                [else 
                    (_treesort (cdr xs) (insert-bst(car xs) (helper _tree (cdr xs)) (car xs) bst))]))
    (flatten-quick (_treesort xs (leaf))))

(check-equal? (insert-bst(car xs) (helper _tree (cdr xs)) 4 test-tree)  (node (node (leaf) 1 (leaf)) 2 (node (node (leaf) 3 
                                        (node (leaf) 4 (leaf))) 4 (node (leaf) 5 (leaf)))))

(check-equal? (treesort '(9 4 8 3 7 5 6 3 1 2 0)) '(0 1 2 3 3 4 5 6 7 8 9))

; --- zad6 ---
(define (delete x tree)
  (cond [(leaf? tree) (leaf)]
        [(= x (node-elem tree)) (cond
            [(leaf? (node-r tree))(node-l tree)]
            [(leaf? (node-l tree))(node-r tree)]
            [else (node
                (node-l tree)
                (car (tree-span (node-r tree)))
                (delete (car (tree-span (node-r tree))) (node-r tree)))])]
        [(< x (node-elem tree)) (node
                                    (delete x (node-l tree))
                                    (node-elem tree)
                                    (node-r tree))]
        [else                   (node
                                    (node-l tree)
                                    (node-elem tree)
                                    (delete x (node-r tree)))]))

#|
(define (delete_brute_force x tree)
    (define (helper _tree xs)
        (cond   [(null? xs) _tree]
                [(= x (car xs)) (helper _tree (cdr xs))]
                [else (insert-bst (car xs) (helper _tree (cdr xs)))]))
    (helper (leaf) (flatten-quick tree)))

(check-equal? (delete_brute_force 5 tree-2) (node (node (leaf) 1 (leaf)) 2 (node (leaf) 3 (node (leaf) 4 (leaf)))))
|#

(define tree-2 (node (node (leaf) 1 (leaf)) 2 (node (node (leaf) 3 (node (leaf) 4 (leaf))) 5 (leaf))))
(check-equal? (delete 5 tree-2) (node (node (leaf) 1 (leaf)) 2 (node (leaf) 3 (node (leaf) 4 (leaf)))))

; --- zad7 ---
(define-struct queue (pref suf) #:transparent)
(define (no-pref q) (null? (queue-pref q)))
(define empty-queue (queue '() '()))
(define (empty? q) (null? (queue-suf q)))

(define (push-back x q) 
    (let ((new_suf (cons x (queue-suf q))))
        (if (no-pref q)
            (queue (reverse new_suf) '())
            (queue (queue-pref q) new_suf))))

(define (front q) (car (queue-pref q)))

(define (pop q) 
    (let ((new_pref (cdr (queue-pref q)))) (cond
        [(null? new_pref) (queue (reverse (queue-suf q)) '())]
        [else (queue new_pref (queue-suf q))])))

(check-equal? empty-queue (pop (pop (pop (push-back 3 (push-back 4 (push-back 5 empty-queue)))))))

; --- zad 8 ---
(define-struct ord (val priority) #:transparent)    ; element

(define-struct hleaf ())
(define-struct hnode (elem rank l r) #:transparent)

(define (heap-make-node elem heap-a heap-b)
    (if (<= (rank heap-a) (rank heap-b))
        (hnode elem (+ 1 (rank heap-a)) heap-b heap-a)
        (hnode elem (+ 1 (rank heap-b)) heap-a heap-b)))

(define (hord? p h)
    (or (hleaf? h)
        (<= p (ord-priority (hnode-elem h)))))

(define (rank h)
    (if (hleaf? h) 0
        (hnode-rank h)))

(define (heap? h)
    (or (hleaf? h)
        (and (hnode? h)
            (heap? (hnode-l h))
            (heap? (hnode-r h))
            (<= (rank (hnode-r h))
                (rank (hnode-l h)))
            (= (hnode-rank h) (+ 1 (hnode-rank (hnode-r h))))

            (hord? (ord-priority (hnode-elem h))
                (hnode-l h))
            (hord? (ord-priority (hnode-elem h))
                (hnode-r h)))))

(define (heap_min_prior heap) (ord-priority (hnode-elem heap)))

(define (heap-merge h1 h2)
    (cond
        [(hleaf? h1) h2]
        [(hleaf? h2) h1]
        [else
            (let   ((h (if (>  (heap_min_prior h1) (heap_min_prior h2)) h1 h2))
                    (H (if (<= (heap_min_prior h1) (heap_min_prior h2)) h1 h2))
                    (e (if (<= (heap_min_prior h1) (heap_min_prior h2)) (hnode-elem h1) (hnode-elem h2))))
                (heap-make-node e (hnode-l H) (heap-merge (hnode-r H) h)))]))

(check-equal?   (hnode-elem
                (heap-merge (hnode (ord "c" 3) 1 (hleaf) (hleaf))
                (heap-merge (hnode (ord "B" 2) 1 (hleaf) (hleaf))
                (hnode (ord "A" 1) 1 (hleaf) (hleaf)))))
                (ord "A" 1))

; --- zad9 ---
(define empty-pq (hleaf))

(define (pq-empty? h)
    (hleaf? h))

(define (pq-insert val h)
    (heap-merge (hnode (ord "x" val) 1 (hleaf) (hleaf)) h))

(define (pq-pop h)
    (if (pq-empty? h) (empty-pq)
        (heap-merge (hnode-l h) (hnode-r h))))
    
(define (pq-min h)
    (if (pq-empty? h) null
    (ord-priority (hnode-elem h))))

(check-equal? (pq-min (pq-insert 2 (pq-insert 1 (pq-insert 2 empty-pq))))
                1)
(check-equal? (pq-empty? empty-pq) #t)

(define (pq-sort xs)
    (define (_drop_in pq elem xs)
        (cond 
            [(null? xs) (pq-insert elem empty-pq)]
            [else (pq-insert elem (_drop_in pq (car xs) (cdr xs)))]))
    (define (_drop_out pq xs)
        (cond
            [(pq-empty? pq) xs]
            [else (cons (pq-min pq) (_drop_out (pq-pop pq) xs))]))
    (if (null? xs) '()
        (_drop_out (_drop_in empty-pq (car xs) (cdr xs)) '())))

(check-equal? (pq-sort '(4 2 6 3 8 4 7 8)) '(2 3 4 4 6 7 8 8))

