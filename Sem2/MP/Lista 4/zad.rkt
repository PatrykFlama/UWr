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


; --- zad2 ---
; (f res-left elem res-right)
(define (fold-tree f elem t)
    (cond   [(leaf? t) elem]
            [(node? t) (f 
                (fold-tree f elem (node-l t))
                (node-elem t) 
                (fold-tree f elem (node-r t)))]))

(define (tree-sum t)
    (fold-tree + 0 t))

(define (tree-flip t)
    (fold-tree (lambda (l val r) (node r val l)) (leaf) t))

(define (tree-height t)
    (fold-tree (lambda (l val r) (+ (max l r) 1 )) 0 t))

(define (tree-span t)
    (cons   (fold-tree (lambda (l val r) (min l val r)) +inf.0 t)
            (fold-tree (lambda (l val r) (max l val r)) -inf.0 t)))

(define (flatten t)
    (fold-tree (lambda (l val r) (append l (cons val r)))
     '() t))

(check-equal? (tree-sum test-tree) 15)
(check-equal? (tree-flip test-tree) (node (node (node (leaf) 5 (leaf)) 4 
                                    (node (leaf) 3 (leaf))) 2 (node (leaf) 1 (leaf))))
(check-equal? (tree-height test-tree) 3)
(check-equal? (tree-span test-tree) '(1.0 . 5.0))
(check-equal? (flatten test-tree) '(1 2 3 4 5))


; --- zad3 ---
(define (bst? t)
    (cond [(leaf? t) #t]
          [(node? t) (and 
            [or (leaf? (node-l t)) 
                (< (node-elem (node-l t)) (node-elem t))]
            [or (leaf? (node-r t)) 
                (> (node-elem (node-r t)) (node-elem t))]
            (bst? (node-l t)) (bst? (node-r t)))]))

(check-equal? (bst? test-tree) #t)

(define (sum-subtree t)
    (cond   [(leaf? t) (leaf)]
            [(node? t) 
                (let [(l (sum-path (node-l t))) (r (sum-path (node-r t)))]
                    (node l (+
                            (if (leaf? l) 0 (node-elem l))
                            (if (leaf? r) 0 (node-elem r))
                            (node-elem t))
                        r))]))

(define (sum-path t)
    (define (_sum-path t acc)
        (cond   [(leaf? t) (leaf)]
                [(node? t)
                    (let ((new-cost (+ acc (node-elem t)))) (node 
                        (_sum-path (node-l t) new-cost)
                        new-cost
                        (_sum-path (node-r t) new-cost)))]))
    (_sum-path t 0))


; --- zad4 ---

