#lang plait

(module+ test
    (print-only-errors #t))

; ----- included template -----
(define (run [s : S-Exp]) : Value
    (error 'run "not implemented"))
; -----------------------------

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
(define-type-alias Value Number)

; environment
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











