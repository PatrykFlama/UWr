#lang plait

(module+ test
    (print-only-errors #t))

;! ----- helper functions -----
(define (s-exp-ref [s : S-Exp] [i : Number]) : S-Exp
    (list-ref (s-exp->list s) i))

(define (contains-duplicates? [xs : (Listof Symbol)]) : Boolean
    (cond
        [(empty? xs) #f]
        [(member (first xs) (rest xs)) #t]
        [else (contains-duplicates? (rest xs))]))

(define (string-append-symbol [s : String] [x : Symbol]) : String
    (string-append s (symbol->string x)))

;! ----- abstract syntax -----
(define-type Op
    (add) (sub) (mul) (leq))

(define-type Program
    (program [ds : (Listof Def)] [e : Exp]))

(define-type Def
    (funD [f : Symbol] [xs : (Listof Symbol)] [e : Exp]))       ;TODO for now functions can take only one argument

(define-type Exp
    (numE [n : Number])
    (varE [x : Symbol])
    (opE  [e1 : Exp] [op : Op] [e2 : Exp])
    (ifzE [e1 : Exp] [e2 : Exp] [e3 : Exp])
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
        [(s-exp-match? `{fun SYMBOL {ANY ...} = ANY} s)
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
        [(s-exp-match? `{ANY SYMBOL ANY} s)
            (opE
                (parse-exp (s-exp-ref s 0))
                (parse-op (s-exp->symbol (s-exp-ref s 1)))
                (parse-exp (s-exp-ref s 2)))] 
        [(s-exp-match? `{ifz ANY then ANY else ANY} s)
            (ifzE (parse-exp (s-exp-ref s 1))
                  (parse-exp (s-exp-ref s 3))
                  (parse-exp (s-exp-ref s 5)))]
        [(s-exp-match? `{let SYMBOL be ANY in ANY} s)
            (letE (s-exp->symbol (s-exp-ref s 1))
                  (parse-exp (s-exp-ref s 3))
                  (parse-exp (s-exp-ref s 5)))]
        [(s-exp-match? `{SYMBOL {ANY ...}} s)
            (appE (s-exp->symbol (s-exp-ref s 0))
                  (map parse-exp (s-exp->list (s-exp-ref s 1))))]
        [else (error 'parse-exp "invalid expression")]))

(define (parse-op [op : Symbol]) : Op
  (cond
    [(eq? op '+) (add)]
    [(eq? op '-) (sub)]
    [(eq? op '*) (mul)]
    [(eq? op '<=) (leq)]
    [else (error 'parse (string-append "unknown operator: " (symbol->string op)))]))


;! ----- interpreter -----
; values
; (define-type-alias Value Number)
(define-type Value
  (numV [n : Number])
  (funV [x : Symbol] [e : Exp] [env : Env]))     ;TODO maybe make a list of symbols from that

; environment ;TODO - not sure about the structure yet
(define-type Storable
  (valS [v : Value])
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
    [empty (error 'lookup (string-append-symbol "unbound variable: " x))]
    [(cons b rst-env) (cond
                        [(eq? x (bind-name b))
                         (bind-ref b)]
                        [else
                         (find-var rst-env x)])]))
  
(define (lookup-env [x : Symbol] [env : Env]) : Value
  (type-case Storable (unbox (find-var env x))
    [(valS v) v]
    [(undefS) (error 'lookup-env "undefined variable")]))
   
(define (update-env! [env : Env] [x : Symbol] [v : Value]) : Void
  (set-box! (find-var env x) (valS v)))

; primitive operations
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

(define (op->proc [op : Op]) : (Value Value -> Value)
  (type-case Op op
    [(add) (op-num-num->proc +)]
    [(sub) (op-num-num->proc -)]
    [(mul) (op-num-num->proc *)]
    [(leq) (op-num-num->proc (lambda (a b) (if (<= a b) 0 1)))]))


; evaluation
(define (eval-program [p : Program]) : Value
    (type-case Program p
        [(program ds e)
         ; first we declare functions existance, so iterate over definitions and add their name to env
         (let ([env (foldl (λ (d env) (extend-env-undef env (funD-f d))) mt-env ds)])     
            ; now we can evaluate definitions and add them to env
            (begin
            (foldl (λ (d dummy) 
                (update-env! env (funD-f d) (funV (first (funD-xs d)) (funD-e d) env)))
                (void) ds)     ;;TODO 1arg usage of function here! ;;TODO should i evaluate it now?
            (eval-exp e env)))]))

(define (eval-exp [e : Exp] [env : Env]) : Value
    (type-case Exp e
        [(numE n) (numV n)]
        [(varE v) (lookup-env v env)]
        [(opE e1 op e2)
            ((op->proc op) (eval-exp e1 env) (eval-exp e2 env))]
        [(ifzE ech ez enz) 
            (if (= 0 (numV-n (eval-exp ech env)))
                (eval-exp ez  env)
                (eval-exp enz env))]
        [(letE x e1 e2)
            (let ([v (eval-exp e1 env)])
                (eval-exp e2 (extend-env env x v)))]
        [(appE f es)
            (apply (lookup-env f env)
                   (map (λ (e) (eval-exp e env)) es))])) 

; TODO actually idk, but thats shit for sure    ;; TODO apply should be zealous
(define (apply [func : Value] [args : (Listof Value)]) : Value      ;;TODO for now arguments has one element
  (type-case Value func
    [(funV x e env)     ;;TODO 1arg here
     (eval-exp e (extend-env env x (first args)))]
    [else (error 'apply "not a function")]))


(define (run [s : S-Exp]) : Value
    (eval-program (parse-program s)))

(module+ test
    (test (run `(define ((fun f1 (x) = (0 + x)) (fun neg? (x) = (0 <= x))) for (ifz (neg? (-1)) then (f1 (10)) else (f1 (-10)))))
          (numV -10))
    (test (run `{define
                    {[fun fact (n) = {ifz n then 1 else {n * {fact ({n - 1})}}}]}
                    for
                    {fact (5)}})
          (numV 120)))