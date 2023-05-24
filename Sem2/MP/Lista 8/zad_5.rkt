#lang plait

(define-type Op-un
  (op-fact) (op-neg))

(define-type Op-bin
  (op-add) (op-sub) (op-mul) (op-div) (op-pow))

(define-type Exp
  (exp-number    [n : Number])
  (exp-unary-op  [op : Op-un]  [e : Exp])
  (exp-binary-op [op : Op-bin] [e1 : Exp] [e2 : Exp]))

(define (parse-op-un s)
  (let ([sym (s-exp->symbol s)])
  (cond
    [(equal? sym '!) (op-fact)]
    [(equal? sym '-) (op-neg)])))

(define (parse-op-bin s)
  (let ([sym (s-exp->symbol s)])
  (cond
    [(equal? sym '+) (op-add)]
    [(equal? sym '-) (op-sub)]
    [(equal? sym '*) (op-mul)]
    [(equal? sym '/) (op-div)]
    [(equal? sym '^) (op-pow)])))

(define (parse-Exp s)
  (cond [(s-exp-number? s) (exp-number (s-exp->number s))]
        [(s-exp-list? s)
            (let ([xs (s-exp->list s)])
            (if (= (length xs) 2) 
                (exp-unary-op (parse-op-un (first xs)) (parse-Exp (second xs)))
                (exp-binary-op (parse-op-bin (first xs)) (parse-Exp (second xs)) (parse-Exp (third xs)))))]))

; ====================================================

(define (exp base exponent)
  (if (<= exponent 0) 1
      (* base (exp base (- exponent 1)))))

(define (factorial n)
  (if (<= n 0) 1
      (* n (factorial (- n 1)))))

(define (eval-op-un op)
  (type-case Op-un op
    [(op-fact) (lambda (n) (factorial n))]
    [(op-neg) (lambda (x) (- 0 x))]))

(define (eval-op-bin op)
  (type-case Op-bin op
    [(op-add) +]
    [(op-sub) -]
    [(op-mul) *]
    [(op-div) /]
    [(op-pow) (lambda (base exponent) (exp base exponent))]))

(define (eval e)
  (type-case Exp e
    [(exp-number n) n]
    [(exp-unary-op op e) ((eval-op-un op) (eval e))]
    [(exp-binary-op op e1 e2) ((eval-op-bin op) (eval e1) (eval e2))]))

(define (calc s)
  (eval (parse-Exp s)))

(calc `(+ 1 2))
(calc `(^ 2 4))
(calc `(! 4))
(calc `(- 4))
