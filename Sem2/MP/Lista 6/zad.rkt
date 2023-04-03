#lang plait

; zad3
(define-type (NNF 'v)
    (nnf-lit [polarity : Boolean] [var : 'v])   ; #f - negation; #t - no negation
    (nnf-conj [l : (NNF 'v)] [r : (NNF 'v)])
    (nnf-disj [l : (NNF 'v)] [r : (NNF 'v)]))

(define example-nnf 
    (nnf-conj 
        (nnf-disj 
            (nnf-lit #t 'p)
            (nnf-lit #f 'p))
        (nnf-conj
            (nnf-lit #t 'q)
            (nnf-lit #f 'q))))

; zad4
(define (neg-nnf f)
    (cond
        [(nnf-lit? f) (nnf-lit (not (nnf-lit-polarity f)) (nnf-lit-var f))]
        [(nnf-conj? f)
            (nnf-disj
                (neg-nnf (nnf-conj-l f))
                (neg-nnf (nnf-conj-r f)))]
        [(nnf-disj? f)
            (nnf-conj
                (neg-nnf (nnf-disj-l f))
                (neg-nnf (nnf-disj-r f)))]))

; zad5
(define (eval-nnf sigma f)
    (cond
        [(nnf-lit? f) 
            (if (nnf-lit-polarity f)
                (nnf-lit-var f)
                (not (nnf-lit-var f)))]
        [(nnf-conj? f) 
            (and
                (eval-nnf sigma (nnf-conj-l f))
                (eval-nnf sigma (nnf-conj-r f)))]
        [(nnf-disj? f) 
            (or
                (eval-nnf sigma (nnf-disj-l f))
                (eval-nnf sigma (nnf-disj-r f)))]))

(define (example-sigma f)
    (cond
        [(equal? f 'p) #t]
        [(equal? f 'p) #f]))

; zad6
(define-type (Formula 'v)
    (var [var : 'v])
    (neg [f : (Formula 'v)])
    (conj [l : (Formula 'v)] [r : (Formula 'v)])
    (disj [l : (Formula 'v)] [r : (Formula 'v)]))

(define (to-nnf f)
    (cond
        [(var? f) (nnf-lit #t (var-var f))]
        [(neg? f) (neg-nnf 
            (to-nnf (neg-f f)))]
        [(conj? f) 
            (nnf-conj
                (to-nnf (conj-l f))
                (to-nnf (conj-r f)))]
        [(disj? f) 
            (nnf-disj
                (to-nnf (disj-l f))
                (to-nnf (conj-r f)))]))

(define example-formula
    (neg (disj
        (neg (conj
            (neg (var 'p))
            (var 'q)))
        (neg (var 'p)))))

; zad7
(define (eval-formula-easy sigma f)
    (eval-nnf sigma (to-nnf f)))

(define (eval-formula sigma f)
    (cond
        [(var? f) (sigma (var-var f))]
        [(neg? f) (not (eval-formula sigma (neg-f f)))]
        [(conj? f) 
            (and
                (eval-formula sigma (conj-l f))
                (eval-formula sigma (conj-r f)))]
        [(disj? f) 
            (or
                (eval-formula sigma (disj-l f))
                (eval-formula sigma (disj-r f)))]))

; zad8
(define (sorted? xs)
    (cond
        [(empty? xs) #t]
        [(empty? (rest xs)) #t]
        [else
            (and 
                (<= (first xs) (first (rest xs)))
                (sorted? (rest xs)))]))        

(define (insert x xs)
    (cond
        [(empty? xs) (cons x '())]
        [(< x (first xs))
            (cons x xs)]
        [else
            (cons 
                (first xs)
                (insert x (rest xs)))]))
