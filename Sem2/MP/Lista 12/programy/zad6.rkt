#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Op
  (add) (sub) (mul) (div) (eql) (leq))

(define-type Exp
  (numE [n : Number])
  (opE [op : Op] [l : Exp] [r : Exp])
  (ifE [b : Exp] [l : Exp] [r : Exp])
  (varE [x : Symbol])
  (letE [x : Symbol] [e1 : Exp] [e2 : Exp])
  (lamE [x : Symbol] [e : Exp])
  (appE [e1 : Exp] [e2 : Exp]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp->symbol
            (first (s-exp->list 
                    (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (opE (parse-op (s-exp->symbol (first (s-exp->list s))))
          (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s))))]
    [(s-exp-match? `{if ANY ANY ANY} s)
     (ifE (parse (second (s-exp->list s)))
          (parse (third (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `SYMBOL s)
     (varE (s-exp->symbol s))]
    [(s-exp-match? `{let SYMBOL ANY ANY} s)
     (letE (s-exp->symbol (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '/) (div)]
    [(eq? op '=) (eql)]
    [(eq? op '<=) (leq)]
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
  (test (parse `{+ 1})
        (appE (varE '+) (numE 1)))
  (test/exn (parse `{^ 1 2})
            "unknown operator")
  (test (parse `{let x 1 {+ x 1}})
        (letE 'x (numE 1) (opE (add) (varE 'x) (numE 1))))
  (test (parse `{lambda {x} 9})
        (lamE 'x (numE 9)))
  (test (parse `{double 9})
        (appE (varE 'double) (numE 9))))


;; eval --------------------------------------

;; values

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean])
  (funV [x : Symbol] [e : Exp] [env : Env]))

(define-type Binding
  (bind [name : Symbol]
        [e : Promise]))

;; thunks

(define-type Thunk
  (valueT [v : Value])
  (thunkT [e : Exp] [env : Env]))

(define-type-alias Promise (Boxof Thunk))

(define (delay [e : Exp] [env : Env]) : Promise
  (box (thunkT e env)))

(define (force [p : Promise]) : Value
  (type-case Thunk (unbox p)
    [(valueT v)
     v]
    [(thunkT e env)
     (let ([v (eval e env)])
       (begin
         (set-box! p (valueT v))
         v))]))

;; environments
; teraz trzymamy środowisko dla każdego przechowywanego wyrażenia

(define-type-alias Env (Listof Binding))
 ; srodowisko przechowuje exp a nie val
(define mt-env empty)
(define (extend-env [env : Env] [x : Symbol] [e : Exp] [eenv : Env]) : Env    ; aby odroczyć obliczenie wartości zmiennej zapisujemy jej środoiwsko
  (cons (bind x (delay e eenv) env)))

(define (lookup-env [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
    [empty (error 'lookup "unbound variable")]
    [(cons b rst-env) (cond
                        [(eq? n (bind-name b))
                        ; (new-eval (bind-e b) (rest env))]  ;; tutaj za każdym razem liczymy wartość zmiennej
                         (force (bind-e b))]   ;; teraz liczymy ją tylko raz
                        [else (lookup-env n rst-env)])]))

;; primitive operations

(define (op-num-num->proc [f : (Number Number -> Number)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (numV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")])))

(define (op-num-bool->proc [f : (Number Number -> Boolean)]) : (Value Value -> Value)
  (λ (v1 v2)
    (type-case Value v1
      [(numV n1)
       (type-case Value v2
         [(numV n2)
          (boolV (f n1 n2))]
         [else
          (error 'eval "type error")])]
      [else
       (error 'eval "type error")])))

(define (op->proc [op : Op]) : (Value Value -> Value)
  (type-case Op op
    [(add) (op-num-num->proc +)]
    [(sub) (op-num-num->proc -)]
    [(mul) (op-num-num->proc *)]
    [(div) (op-num-num->proc /)]
    [(eql) (op-num-bool->proc =)]
    [(leq) (op-num-bool->proc <=)]))

;; evaluation function (eval/apply)



(define (new-eval [e : Exp] [env : Env]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(opE o l r) ((op->proc o) (new-eval l env) (new-eval r env))]
    [(ifE b l r)
     (type-case Value (new-eval b env)
       [(boolV v)
        (if v (new-eval l env) (new-eval r env))]
       [else
        (error 'new-eval "type error")])]
    [(varE x) ; tu obliczamy
     (lookup-env x env)]
    [(letE x e1 e2)
       (new-eval e2 (extend-env env x e1 env))]     ; e2 musimy policzyć, bo jest to nasze główne wyrażenie
                                                    ; z e1 czekamy dopóki nie będzie potrzebne w e2
    [(lamE x b)
     (funV x b env)]
    [(appE e1 e2)
     (apply (new-eval e1 env) e2 env)]))  ; e1 musimy policzyć, natomiast z e2 czekamy dopóki nie będzie potrzebne
                                          ; (odroczenie odbywa się w extend-env)

(define (apply [v1 : Value] [e2 : Exp] [env : Env]) : Value
  (type-case Value v1
    [(funV x b env)
     (new-eval b (extend-env env x e2 env))] 
    [else (error 'apply "not a function")]))

(define (run [e : S-Exp]) : Value
  (new-eval (parse e) mt-env))

;test leta
;(run `(let x (+ 1 2) (let y (+ 2 x) (+ y x))))

;(run `((lambda (x) (+ 3 x)) 5)) 

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
        (numV 8))
  (test (run `{let x 1 {+ x 1}})
        (numV 2))
  (test (run `{let x 1 {+ x {let y 2 {* x y}}}})
        (numV 3))
  (test (run `{let x 1
                {+ x {let x {+ x 1}
                       {* x 3}}}})
        (numV 7))
  (test (run `{{lambda {x} {+ x 1}} 5})
        (numV 6))
  (test (run `{lambda {x} {lambda {y} {+ x y}}})
        (funV 'x (lamE 'y (opE (add) (varE 'x) (varE 'y))) mt-env)))

;; printer ———————————————————————————————————-

(define (value->string [v : Value]) : String
  (type-case Value v
    [(numV n) (to-string n)]
    [(boolV b) (if b "true" "false")]
    [(funV x e env) "#<procedure>"]))

(define (print-value [v : Value]) : Void
  (display (value->string v)))

(define (main [e : S-Exp]) : Void
  (print-value (new-eval (parse e) mt-env)))