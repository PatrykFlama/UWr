#lang plait

(define-type Op
  (op-add) (op-mul) (op-sub) (op-div))

(define-type Exp
  (exp-number [n : Number])
  (exp-op [op : Op] [e1 : Exp] [e2 : Exp]))

(define (eval-op op)
  (let ([sym (s-exp->symbol op)])
  (cond
    [(equal? sym '+) +]
    [(equal? sym '-) -]
    [(equal? sym '*) *]
    [(equal? sym '/) /])))

(define (calc-Exp s)
  (cond
    [(s-exp-number? s) (s-exp->number s)]
    [(s-exp-list? s)
     (let ([xs (s-exp->list s)])
         ((eval-op  (first xs)) 
          (calc-Exp (second xs))
          (calc-Exp (third  xs))))]))

