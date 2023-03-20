#lang racket
(require rackunit)

; --- zad1 ---
#|
ANS
|#

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

(define (tree-span-bst tree)
    (cons   (fold-tree (lambda (l val r) (min l val r)) +inf.0 tree)
            (fold-tree (lambda (l val r) (max l val r)) -inf.0 tree)))

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
(define (insert-bst x tree)
  (cond [(leaf? tree) (node (leaf) x (leaf))]
        [(node? tree)
         (cond  [(<= x (node-elem tree))
                    (node (insert-bst x (node-l tree))
                          (node-elem tree)
                          (node-r tree))]
                [else
                    (node (node-l tree)
                          (node-elem tree)
                          (insert-bst x (node-r tree)))])]))

(define (treesort xs)
    (define (_treesort xs bst)
        (cond   [(null? xs) bst]
                [else 
                    (_treesort (cdr xs) (insert-bst (car xs) bst))]))
    (flatten-quick (_treesort xs (leaf))))

(check-equal? (insert-bst 4 test-tree)  (node (node (leaf) 1 (leaf)) 2 (node (node (leaf) 3 
                                        (node (leaf) 4 (leaf))) 4 (node (leaf) 5 (leaf)))))

(check-equal? (treesort '(9 4 8 3 7 5 6 3 1 2 0)) '(0 1 2 3 3 4 5 6 7 8 9))

; --- zad6 🤮 ---
(define (delete x tree)
    (define (_find_left tree)
        (fold-tree (lambda (l val r) (cond
                                                [(not (leaf? l)) l]
                                                [(not (leaf? r)) r]
                                                [else val]))
                        (leaf) tree))

    (define (_delete_left left-subtree)    ; get val of left leaf and delete it
        (cond   [(leaf? left-subtree) (leaf)]
                [else
                (let ((left (node-l left-subtree)) (right (node-r left-subtree)))
                (if (leaf? left)
                    (if (leaf? right) right (leaf))
                    (node (_delete_left left) (node-elem left-subtree) right)))]))

    (cond   [(leaf? tree) (leaf)]
            [(node? tree)
                (cond [(= x (node-elem tree))
                        (cond 
                            [(node? (node-r tree))
                                (let ((val (_delete_left (node-r tree))))
                                    (if (leaf? val) 
                                        (leaf)
                                        (node 
                                            (node-l tree) 
                                            (_find_left (node-r tree)) 
                                            val)))]
                            [(node? (node-l tree))
                                (node-l tree)]
                            [else (leaf)])]
                      [(< x (node-elem tree))
                        (node
                            (delete x (node-l tree))
                            (node-elem tree)
                            (node-r tree))]
                      [else
                        (node
                            (node-l tree)
                            (node-elem tree)
                            (delete x (node-r tree)))])]))

(define tree-2 (node (node (leaf) 1 (leaf)) 2 (node (node (leaf) 3 (node (leaf) 4 (leaf))) 5 (leaf))))
(check-equal? (delete 5 tree-2) (node (node (leaf) 1 (leaf)) 2 (node (leaf) 3 (node (leaf) 4 (leaf)))))
(delete 3 tree-2)

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

(define myq empty-queue)
