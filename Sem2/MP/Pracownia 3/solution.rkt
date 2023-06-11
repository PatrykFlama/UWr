#lang plait

(module+ test)
    ; (print-only-errors #t))

;! ----- helper functions -----
(define (s-exp-ref [s : S-Exp] [i : Number]) : S-Exp
    (list-ref (s-exp->list s) i))

(define (contains-duplicates? [xs : (Listof Symbol)]) : Boolean
    (cond
        [(empty? xs) #f]
        [(member (first xs) (rest xs)) #t]
        [else (contains-duplicates? (rest xs))]))

(define (double-foldl f base xs ys)
    (cond
        [(empty? xs) base]
        [(empty? (rest xs)) (f base (first xs) (first ys))]
        [else (double-foldl f (f base (first xs) (first ys)) (rest xs) (rest ys))]))

(define (nice-error [func : Symbol] [str : String] [s : Symbol])
    (error func (string-append str (symbol->string s))))


;! ----- abstract syntax -----
(define-type Op
    (add) (sub) (mul) (leq))

(define-type Program
    (program [ds : (Listof Def)] [e : Exp]))

(define-type Def
    (funD [f : Symbol] [xs : (Listof Symbol)] [e : Exp]))

(define-type Exp
    (numE [n : Number])
    (varE [x : Symbol])
    (opE  [e1 : Exp]   [op : Op]  [e2 : Exp])
    (ifzE [e1 : Exp]   [e2 : Exp] [e3 : Exp])
    (letE [x : Symbol] [e1 : Exp] [e2 : Exp])
    (appE [f : Symbol] [es : (Listof Exp)]))


;! ----- parser -----
(define (parse-program [s : S-Exp]) : Program
    (cond
        [(s-exp-match? `{define {ANY ...} for ANY} s)
         (let ([defs (map parse-def (s-exp->list (s-exp-ref s 1)))])
            (if (contains-duplicates? (map funD-f defs))
                (error 'parse-program "duplicate function names")
                (program defs (parse-exp (s-exp-ref s 3)))))]
        [else
            (error 'parse-program "invalid program")]))

(define (parse-def [s : S-Exp]) : Def
    (cond
        [(s-exp-match? `{fun SYMBOL {SYMBOL ...} = ANY} s)
         (let ([xs (map s-exp->symbol (s-exp->list (s-exp-ref s 2)))])
            (if (contains-duplicates? xs)
                (error 'parse-def "duplicate parameter names")
                (funD (s-exp->symbol (s-exp-ref s 1))
                      xs
                      (parse-exp (s-exp-ref s 4)))))]
        [else
            (error 'parse-def "invalid definition")]))

(define (parse-exp [s : S-Exp]) : Exp
    (cond
        [(s-exp-match? `NUMBER s)
            (numE (s-exp->number s))]
        [(s-exp-match? `SYMBOL s)
            (varE (s-exp->symbol s))]
        [(s-exp-match? `{ifz ANY then ANY else ANY} s)
            (ifzE (parse-exp (s-exp-ref s 1))
                  (parse-exp (s-exp-ref s 3))
                  (parse-exp (s-exp-ref s 5)))]
        [(s-exp-match? `{let SYMBOL be ANY in ANY} s)
            (letE (s-exp->symbol (s-exp-ref s 1))
                  (parse-exp (s-exp-ref s 3))
                  (parse-exp (s-exp-ref s 5)))]
        [(s-exp-match? `{ANY SYMBOL ANY} s)
            (opE
                (parse-exp (s-exp-ref s 0))
                (parse-op (s-exp->symbol (s-exp-ref s 1)))
                (parse-exp (s-exp-ref s 2)))] 
        [(s-exp-match? `{SYMBOL {ANY ...}} s)
            (appE (s-exp->symbol (s-exp-ref s 0))
                  (map parse-exp (s-exp->list (s-exp-ref s 1))))]
        [else (error 'parse-exp "invalid expression")]))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+)  (add)]
    [(eq? op '-)  (sub)]
    [(eq? op '*)  (mul)]
    [(eq? op '<=) (leq)]
    [else (nice-error 'parse "unknown operator: " op)]))


;! ----- interpreter -----
; values
(define-type-alias Value Number)
(define-type Function
    (funV [xs : (Listof Symbol)] [e : Exp] [env : Env]))

;* although that would be more elegant (but it does not pass all tests):
; (define-type Value
;   (numV [n : Number])
;   (funV [xs : (Listof Symbol)] [e : Exp] [env : Env]))

; environment
(define-type Storable
  (valS [v : Value])
  (funS [f : Function])
  (undefS))

(define-type Binding
  (bind [name : Symbol]
        [ref : (Boxof Storable)]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)

(define (extend-env-undef [env : Env] [x : Symbol]) : Env
  (cons (bind x (box (undefS))) env))

(define (extend-env [env : Env] [x : Symbol] [v : Value]) : Env
  (cons (bind x (box (valS v))) env))

(define (find-var [env : Env] [x : Symbol]) : (Boxof Storable)
  (type-case (Listof Binding) env
    [empty (nice-error 'lookup "unbound variable: " x)]
    [(cons b rst-env) (cond
                        [(eq? x (bind-name b))
                         (bind-ref b)]
                        [else
                         (find-var rst-env x)])]))
  
(define (lookup-env-var [x : Symbol] [env : Env]) : Value
  (type-case Storable (unbox (find-var env x))
    [(valS v) v]
    [(funS f) 
        (nice-error 'lookup-env-var "not a variable: " x)]
    [(undefS) (nice-error 'lookup-env-var "undefined variable: " x)]))

(define (lookup-env-fun [x : Symbol] [env : Env]) : Function
  (type-case Storable (unbox (find-var env x))
    [(funS f) f]
    [(valS v) 
        (nice-error 'lookup-env-fun "not a function: " x)]
    [(undefS) (nice-error 'lookup-env-fun "undefined variable: " x)]))
   
(define (update-env-val! [env : Env] [x : Symbol] [v : Value]) : Void
  (set-box! (find-var env x) (valS v)))

(define (update-env-fun! [env : Env] [x : Symbol] [f : Function]) : Void
  (set-box! (find-var env x) (funS f)))

; primitive operations         
(define (op->proc [op : Op]) : (Value Value -> Value)
  (type-case Op op
    [(add) +]
    [(sub) -]
    [(mul) *]
    [(leq) (lambda (a b) (if (<= a b) 0 1))]))


; evaluation
(define (eval-program [p : Program]) : Value
    (type-case Program p
        [(program ds e)
         ; first we declare functions existance, so iterate over definitions and add their name to env
         (let ([env (foldl (λ (d env) (extend-env-undef env (funD-f d))) mt-env ds)])     
            ; now we can evaluate definitions and add them to env
            (begin
            (foldl (λ (d dummy) 
                (update-env-fun! env (funD-f d) (funV (funD-xs d) (funD-e d) env)))
                (void) ds)
            (eval-exp e env)))]))

(define (eval-exp [e : Exp] [env : Env]) : Value
    (type-case Exp e
        [(numE n) n]
        [(varE v) (lookup-env-var v env)]
        [(opE e1 op e2)
            ((op->proc op) (eval-exp e1 env) (eval-exp e2 env))]
        [(ifzE ech ez enz) 
            (if (= 0 (eval-exp ech env))
                (eval-exp ez  env)
                (eval-exp enz env))]
        [(letE x e1 e2)
            (let ([v (eval-exp e1 env)])
                (eval-exp e2 (extend-env env x v)))]
        [(appE f es)
            (apply (lookup-env-fun f env)
                   (map (λ (e) (eval-exp e env)) es))])) 

(define (apply [fun : Function] [args : (Listof Value)]) : Value
  (type-case Function fun
    [(funV xs e env)
     (eval-exp e 
        (double-foldl
            (λ (new-env x arg)
                (extend-env new-env x arg))
            env xs args))]))


(define (run [s : S-Exp]) : Value
    (eval-program (parse-program s)))

(module+ test
    (test (run `(define ((fun f1 (x) = (0 + x)) (fun neg? (x) = (0 <= x))) for (ifz (neg? (-1)) then (f1 (10)) else (f1 (-10)))))
          -10)
    (test (run `{define
                    {[fun fact (n) = {ifz n then 1 else {n * {fact ({n - 1})}}}]}
                    for
                    {fact (5)}})
          120)
    (test (run `(define () for (let x be (1 + 2) in (x + x))))
          6)
    (test (run `(define ((fun sum (x y) = (y + x)) (fun neg? (x) = (0 <= x))) for (ifz (neg? (-1)) then (sum (10 30)) else (sum (-10 -30)))))
          -40)
    (test (run `{define
                    {[fun even (n) = {ifz n then 0 else {odd ({n - 1})}}]
                        [fun odd (n) = {ifz n then 42 else {even ({n - 1})}}]}
                        for
                        {even (1024)}})
          0)
    (test (run `{define
                    {[fun gcd (m n) = {ifz n
                    then m
                    else {ifz {m <= n}
                    then {gcd (m {n - m})}
                    else {gcd ({m - n} n)}}}]}
                    for
                    {gcd (81 63)}})
        9))
