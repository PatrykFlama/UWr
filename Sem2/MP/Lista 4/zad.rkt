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

(define (tree-span tree)
    (cons   (fold-tree (lambda (l val r) (min l val r)) +inf.0 tree)
            (fold-tree (lambda (l val r) (max l val r)) -inf.0 tree)))

(define (flatten tree)
    (fold-tree (lambda (l val r) (append l (cons val r))) '() tree))

(check-equal? (tree-sum test-tree) 15)
(check-equal? (tree-flip test-tree) (node (node (node (leaf) 5 (leaf)) 4 
                                    (node (leaf) 3 (leaf))) 2 (node (leaf) 1 (leaf))))
(check-equal? (tree-height test-tree) 3)
(check-equal? (tree-span test-tree) '(1.0 . 5.0))
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

; --- zad6 ---
(define (delete x tree)
  (define (_delete val tree)
    (cond [(leaf? tree) val]
          [(node? tree) (node   (_delete val (node-l tree))
                                (node-elem tree)
                                (node-r tree))]))
  (cond [(leaf? tree) (leaf)]
        [(node? tree)
         (cond [(= x (node-elem tree))
                (_delete (node-l tree) (node-r tree))]
               [(< x (node-elem tree))
                (node (delete x (node-l tree))
                      (node-elem tree)
                      (node-r tree))]
               [else (node (node-l tree)
                           (node-elem tree)
                           (delete x (node-r tree)))])]))
