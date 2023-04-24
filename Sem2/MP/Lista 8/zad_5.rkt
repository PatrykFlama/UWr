#lang plait

(define (fact n)
    (local (
    (define (_fact x acc)
        (if (<= x 1) acc
            (_fact (- x 1) (* acc x)))))
    (if (= n 0) 1
        (_fact n 1))))

(define (expt a b)
    (cond
        [(= b 0) 1]
        [(= b 1) a]
        [else (* a (expt a (- b 1)))]))
(define add +)
(define sub -)
(define mul *)
(define div /)
; ----------------------------------------------
(define-type Op-bin
  (op-add) (op-mul) (op-sub) (op-div) (op-expt))
(define-type Op-un (op-fact) (op-neg))

(define-type Exp
  (exp-number [n : Number])
  (exp-op-bin [op : Op-bin] [e1 : Exp] [e2 : Exp])
  (exp-op-un  [op : Op-un]  [e : Exp]))

(define (parse-Op-bin s)
  (let ([sym (s-exp->symbol s)])
  (cond
    [(equal? sym '+) (op-add)]
    [(equal? sym '-) (op-sub)]
    [(equal? sym '*) (op-mul)]
    [(equal? sym '/) (op-div)]
    [(equal? sym '^) (op-expt)])))
(define (parse-Op-un s)
  (let ([sym (s-exp->symbol s)])
  (cond
    [(equal? sym '!) (op-fact)]
    [(equal? sym '-) (op-neg)])))

(define (parse-Exp s)
  (cond
    [(s-exp-number? s) (exp-number (s-exp->number s))]
    [(s-exp-list? s)
     (let ([xs (s-exp->list s)])
     (if (= (length xs) 2)
         (exp-op-un  (parse-Op-un (first xs))
                     (parse-Exp (second xs)))
         (exp-op-bin (parse-Op-bin (first xs))
                     (parse-Exp (second xs))
                     (parse-Exp (third xs)))))]))

; ==============================================

(define (eval-op-bin op)
  (type-case Op-bin op
    [(op-add) add]
    [(op-sub) sub]
    [(op-mul) mul]
    [(op-div) div]
    [(op-expt) expt]))
; (define (eval-op-un op)
;   (type-case Op-un op
;     [(op-fact) -]
;     [(op-neg)  -]))

(define (eval e)
  (type-case Exp e
    [(exp-number n)    n]
    [(exp-op-bin op e1 e2)
        ((eval-op-bin op) (eval e1) (eval e2))]
    [(exp-op-un op e) 0]))
        ; ((eval-op-un op) (eval e))]))

