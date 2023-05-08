#lang racket
(require (only-in plait s-exp-match?))

(provide (contract-out
    [run-parser (-> (listof (cons/c check-pattern (procedure? any/c)))
                    check-sexp
                    any/c)]))

(define (check-pattern p)
  (match p
    ['ANY #t]
    ['SYMBOL #t]
    ['NUMBER #t]
    ['() #t]
    [(cons p1 p2)
     (and (check-pattern p1) (check-pattern p2))]
    [_ #f]))

(define (check-sexp s)
    (s-exp-match? `(ANY) s))

; te funkcje dotyczą tylko parsowania, nie wyrażeń arytmentycznych
(define (match-sexp pat s)
  (match pat
    ['ANY    (list s)]
    ['SYMBOL (and (symbol? s) (list s))]
    ['NUMBER (and (number? s) (list s))]
    ['()     (and (null? s)   null)]
    [(cons p1 p2)
     (and (pair? s)
          (let ([r1 (match-sexp p1 (car s))])
            (and r1
                 (let ([r2 (match-sexp p2 (cdr s))])
                   (and r2 (append r1 r2))))))]
    [_
     (cond
       [(symbol? pat) (and (symbol? s) (eq? pat s) null)])]))

(define (run-parser p s)
  (match p
    ['() (error "Syntax error")]
    [(cons (list pat action) rest-p)
     (let ([r (match-sexp pat s)])
       (if r
           (apply action r)
           (run-parser rest-p s)))]))
