#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Op
  (add)
  (sub)
  (mul)
  (div)
  (eql)
  (leq)

  (lcons)
  (lcar)
  (lcdr)
  (lisnull))

(define-type Exp
  (numE [n : Number])
  (opE [op : Op]
       [l : Exp]
       [r : Exp])
  (opEun [op : Op]
         [l : Exp])
  (ifE [b : Exp]
       [l : Exp]
       [r : Exp])
  ; (condE [cs : (Listof (Exp * Exp))])
  (trueE)
  (falseE)
  (listE [ls : (Listof Exp)]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `#t s) (trueE)]
    [(s-exp-match? `#f s) (falseE)]
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    ; [(s-exp-match? `{cond ANY ...} s)
    ;  (condE (parse-cond (rest (s-exp->list s))))]

    [(s-exp-match? `{null} s)
      (listE empty)]
    [(s-exp-match? `{list ANY ...} s)
      (listE (parse-list (rest (s-exp->list s))))]

    [(s-exp-match? `{SYMBOL ANY} s)
      (opEun 
        (parse-op (s-exp->symbol (first (s-exp->list s))))
        (parse (second (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (opE (parse-op (s-exp->symbol (first (s-exp->list s))))
          (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-list [ss : (Listof S-Exp)]) : (Listof Exp)
  (type-case (Listof S-Exp) ss
    [empty empty]
    [(cons s ss)
      (cons (parse s)
            (parse-list ss))]))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '/) (div)]
    [(eq? op '=) (eql)]
    [(eq? op '<=) (leq)]
    [(eq? op 'cons) (lcons)]
    [(eq? op 'car) (lcar)]
    [(eq? op 'cdr) (lcdr)]
    [(eq? op 'null?) (lisnull)]
    [else (error 'parse "unknown operator")]))
                
(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `{+ 2 1})
        (opE (add) (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (opE (mul) (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (opE (add)
             (opE (mul) (numE 3) (numE 4))
             (numE 8)))
  (test (parse `{if {= 0 1} {* 3 4} 8})
        (ifE (opE (eql) (numE 0) (numE 1))
             (opE (mul) (numE 3) (numE 4))
             (numE 8)))
   (test/exn (parse `{{+ 1 2}})
            "invalid input")
  (test/exn (parse `{+ 1})
            "invalid input")
  (test/exn (parse `{^ 1 2})
            "unknown operator"))
  ; (test (parse `{cond {{= 0 1} {* 3 4}}
  ;                     {{= 1 1} 8}})
  ;       (condE (list (pair (opE (eql) (numE 0) (numE 1))
  ;                          (opE (mul) (numE 3) (numE 4)))
  ;                    (pair (opE (eql) (numE 1) (numE 1))
  ;                          (numE 8))))))
  
;; eval --------------------------------------

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean])
  (listV [ls : (Listof Value)]))

(define (op-num-num->proc [f : (Number Number -> Number)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (numV (f n1 n2))]
         [else
          (error 'op-num-num->proc "type error")])]
      [else
       (error 'op-num-num->proc "type error")])))

(define (op-num-bool->proc [f : (Number Number -> Boolean)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (boolV (f n1 n2))]
         [else
          (error 'op-num-bool->proc "type error")])]
      [else
       (error 'op-num-bool->proc "type error")])))

(define (op-val-list->proc f) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v2
      [(listV ls)
       (listV (f v1 ls))]
      [else
       (error 'op-val-list->proc "type error")])))

(define (op-list->bool->proc [f : ((Listof Value) -> Boolean)]) : (Value -> Value)
  (λ (v)
    (type-case Value v
      [(listV ls)
       (boolV (f ls))]
      [else
       (error 'op-list->bool->proc "type error")])))

(define (op-list->proc f) : (Value -> Value)
  (λ (v)
    (type-case Value v
      [(listV ls)
       (f ls)]
      [else
       (error 'op-list->proc "type error")])))

(define (op-list->list->proc f) : (Value -> Value)
  (λ (v)
    (type-case Value v
      [(listV ls)
       (listV (f ls))]
      [else
       (error 'op-list->list->proc "type error")])))

(define (op->proc [op : Op]) : (Value Value -> Value)
  (type-case Op op
    [(add) (op-num-num->proc +)]
    [(sub) (op-num-num->proc -)]
    [(mul) (op-num-num->proc *)]
    [(div) (op-num-num->proc /)]
    [(eql) (op-num-bool->proc =)]
    [(leq) (op-num-bool->proc <=)]
    [(lcons) (op-val-list->proc cons)]
    [else (error 'op->proc "error")]))

(define (opun->proc [op : Op]) : (Value -> Value)
  (type-case Op op
    [(lcar) (op-list->proc first)]
    [(lcdr) (op-list->list->proc rest)]
    [(lisnull) (op-list->bool->proc empty?)]
    [else (error 'opun->proc "error")]))

(define (eval [e : Exp]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(trueE) (boolV #t)]
    [(falseE) (boolV #f)]
    [(opE o l r) ((op->proc o) (eval l) (eval r))]
    [(ifE b l r)
     (type-case Value (eval b)
       [(boolV v)
        (if v (eval l) (eval r))]
       [else
        (error 'eval "type error")])]
    [(listE ls) (listV (map eval ls))]
    [(opEun o ls) ((opun->proc o) (eval ls))]))
    ; [(condE cs)
    ;  (eval (cond->if cs))]))

; (define (cond->if [cs : (Listof (Exp * Exp))]) : Exp
;   (type-case (Listof (Exp * Exp)) cs
;     [empty
;      (numE 42)]
;     [(cons c cs)
;      (ifE (fst c)
;           (snd c )
;           (cond->if cs))]))

(define (run [e : S-Exp]) : Value
  (eval (parse e)))

(module+ test
  (test (run `2)
        (numV 2))
  (test (run `{+ 2 1})
        (numV 3))
  (test (run `{* 2 1})
        (numV 2))
  (test (run `{+ {* 2 3} {+ 5 8}})
        (numV 19))
  (test (run `{= 0 1})
        (boolV #f))
  (test (run `{if {= 0 1} {* 3 4} 8})
        (numV 8)))
  ; (test (run `{cond {{= 0 1} {* 3 4}}
  ;                   {{= 1 1} 8}})
        ; (numV 8)))

;; printer ———————————————————————————————————-

(define (value->string [v : Value]) : String
  (type-case Value v
    [(numV n) (to-string n)]
    [(boolV b) (if b "true" "false")]
    ; [(listV ls) (format "(list ~a)" (map value->string ls))]))
    [(listV ls) "list, indeed"]))

(define (print-value [v : Value]) : Void
  (display (value->string v)))

(define (main [e : S-Exp]) : Void
  (print-value (eval (parse e))))

;; abstract machine ---------------------------

(define-type Stack
  (emptyS)
  (rightS [op : Op] [exp : Exp] [s : Stack])
  (leftS [op : Op] [val : Value] [s : Stack])
  (ifS [exp1 : Exp] [exp2 : Exp] [s : Stack]))

(define (evalAM [e : Exp] [s : Stack]) : Value
  (type-case Exp e
    [(numE n)
     (continueAM s (numV n))]
    [(trueE)
     (continueAM s (boolV #t))]
    [(falseE)
     (continueAM s (boolV #f))]
    [(listE ls)
     (continueAM s (listV (map (lambda (x) (evalAM x (emptyS))) ls)))]
    [(opE o e1 e2)
     (evalAM e1 (rightS o e2 s))]
    [(opEun o e)
      (evalAM e (leftS o (listV '()) s))]
    [(ifE condition e1 e2) 
     (evalAM condition (ifS e1 e2 s))]))

(define (continueAM [s : Stack] [v : Value]) : Value
  (type-case Stack s
    [(emptyS)
     v]
    [(rightS op e s)
     (evalAM e (leftS op v s))]
    [(leftS op u s)
      (if (and (listV? u) (empty? (listV-ls u)))
          (continueAM s ((opun->proc op) v))
          (continueAM s ((op->proc op) u v)))]
    [(ifS e1 e2 s)
     (type-case Value v
       [(boolV condition) (if condition
            (evalAM e1 s)
            (evalAM e2 s))]
       [else
        (error 'continueAM "if expression type error")])]))
  
(define (runAM [e : S-Exp]) : Value
  (evalAM (parse e) (emptyS)))

(module+ test
  (test (run `2)
        (runAM `2))
  (test (run `{+ 2 1})
        (runAM `{+ 2 1}))
  (test (run `{* 2 1})
        (runAM `{* 2 1}))
  (test (run `{+ {* 2 3} {+ 5 8}})
        (runAM `{+ {* 2 3} {+ 5 8}})))

