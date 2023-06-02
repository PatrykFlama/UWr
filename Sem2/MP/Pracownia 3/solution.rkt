#lang plait

(module+ test
    (print-only-errors #t))

;! ----- helper functions -----
(define (s-exp-ref [s : S-Exp] [i : Natural]) : S-Exp
    (list-ref (s-exp->list s) i))

(define (contains-duplicates? [xs : (Listof Symbol)]) : Boolean
    (cond
        [(empty? xs) #f]
        [(member (first xs) (rest xs)) #t]
        [else (contains-duplicates? (rest xs))]))

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
                (program defs (parse-exp (s-exp-ref s 2)))))]
        [else
            (error 'parse-program (string-append "invalid program: " (s-exp->string s)))]))

(define (parse-def [s : S-Exp]) : Def
    (cond
        [(s-exp-match? `{fun SYMBOL {ANY ...} = ANY} s)
         (let ([xs (map s-exp->symbol (s-exp->list (s-exp-ref s 2)))])
            (if (contains-duplicates? xs)
                (error 'parse-def "duplicate parameter names")
                (funD (s-exp->symbol (s-exp-ref s 1))
                      xs
                      (parse-exp (s-exp-ref s 3)))))]
        [else
            (error 'parse-def (string-append "invalid definition: " (s-exp->string s)))]))

(define (parse-exp [s : S-Exp]) : Exp
    (cond
        [(s-exp-match? `NUMBER s)
            (numE (s-exp->number s))]
        [(s-exp-match? SYMBOL s)
            (varE (s-exp->symbol s))]
        [(s-exp-match? `{ANY SYMBOL ANY} s)
            (opE
                (parse-exp (s-exp-ref s 0))
                (parse-op (s-exp->symbol (s-exp-ref s 1)))
                (parse-exp (s-exp-ref s 2)))] 
        [(s-exp-match? `{ifz ANY ANY ANY} s)
            (ifzE (parse-exp (s-exp-ref s 1))
                  (parse-exp (s-exp-ref s 2))
                  (parse-exp (s-exp-ref s 3)))]
        [(s-exp-match? `{let {SYMBOL} = ANY in ANY} s)
            (letE (s-exp->symbol (s-exp-ref s 1))
                  (parse-exp (s-exp-ref s 2))
                  (parse-exp (s-exp-ref s 3)))]
        [(s-exp-match? `{SYMBOL {ANY ...}} s)
            (appE (s-exp->symbol (s-exp-ref s 0))
                  (map parse-exp (s-exp->list (s-exp-ref s 1))))]
        [else (error 'parse-exp (string-append "invalid expression: " (s-exp->string s)))]))

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
  (boolV [b : Boolean])
  (funV [x : Symbol] [e : Exp] [env : Env])     ;TODO maybe make a list of symbols from that
  (primopV [f : (Value -> Value)]))

; environment ;TODO - not sure about the structure yet
(define-type Binding
  (bind [name : Symbol]
        [val : (-> Value)]))

(define-type-alias Env (Listof Binding))

(define empty-env empty)
(define (extend-env [env : Env] [x : Symbol] [t : (-> Value)]) : Env
    (cons (bind x t) env))
(define (lookup-env [n : Symbol] [env : Env]) : Value
    (type-case (Listof Binding) env
        [empty (error 'lookup "unbound variable")]
        [(cons b rst-env) (cond [(eq? n (bind-name b)) ((bind-val b))]
                                [else (lookup-env n rst-env)])]))

; ---
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
    [empty (error 'lookup "unbound variable")]
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
    [(leq) (op-num-bool->proc (λ (a b) (if (<= a b) 0 1)))]))


; evaluation
;TODO calculate definitions and add them to the environment, then eval main exp with that env
(define (eval-program [p : Program]) : Value
    (type-case Program p
        [(program ds e)
         (let ([env (foldl (λ (d env) (eval-def d env)) mt-env ds)])    
            (eval-exp e env))]))

; TODO eval expression in given evnironment
(define (eval-exp [e : Exp] [env : Env]) : Value
    (type-case Exp e
        [(numE n) n]
        [(varE v) (lookup-env v env)]
        [(opE e1 op e2)
            ((op->proc op) (eval-exp e1 env) (eval-exp e2 env))]
        [(ifzE e1 e2 e3)
            (if (= 0 (eval-exp e1 env))
                (eval-exp e1 env)
                (eval-exp e2 env))]
        [(letE x e1 e2)
            (eval-exp e2 (extend-env env x (eval-exp e1 env)))]
        [(appE f es)
            (apply (lookup-env f env) (map (λ (e) (eval-exp e env)) es))])) 

; TODO actually idk, but thats shit for sure
(define (apply [v : Value] [vs : (Listof Value)]) : Value
  (type-case Value v1
    [(funV x e env)
     (eval-exp e (extend-env env x v2))]
    [else (error 'apply "not a function")]))


(define (run [s : S-Exp]) : Value
  (error "What about NO?"))

