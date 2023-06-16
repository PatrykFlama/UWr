#lang plait

(module+ test
  (print-only-errors #t))

;; abstract syntax -------------------------------

(define-type Exp
  (numE [n : Number])
  (boolE [b : Boolean])
  (ifE [e0 : Exp] [e1 : Exp] [e2 : Exp])
  (varE [x : Symbol])
  (letE [x : Symbol] [e1 : Exp] [e2 : Exp])
  (lamE [x : Symbol] [e : Exp])
  (appE [e1 : Exp] [e2 : Exp]))

;; parse ----------------------------------------

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s)
     (numE (s-exp->number s))]
    [(s-exp-match? `true s)
     (boolE #t)]
    [(s-exp-match? `false s)
     (boolE #f)]
    [(s-exp-match? `{lambda {SYMBOL ...} ANY} s)
     (make-funs  
      (map s-exp->symbol (s-exp->list (second (s-exp->list s))))
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
    [(s-exp-match? `{ANY ANY ...} s)
     (make-apps (parse (first (s-exp->list s)))
                (map parse (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (make-funs ids e)
  (if (empty? ids)
      e
      (lamE (first ids)
            (make-funs (rest ids) e))))

(define (make-apps e es)
  (if (empty? es)
      e
      (make-apps (appE e (first es))
                 (rest es))))

(module+ test
  (test (parse `{lambda {x y} {+ x y}})
        (lamE
         'x
         (lamE
          'y
          (appE
           (appE
            (varE '+)
            (varE 'x))
           (varE 'y))))))

;; environments -----------------------------------

(define-type (Binding 'a)
  (bind [name : Symbol]
        [val : 'a]))

(define-type-alias (Env 'a) (Listof (Binding 'a)))

(define mt-env empty)
(define (extend-env [env : (Env 'a)] [x : Symbol] [v : 'a]) : (Env 'a)
  (cons (bind x v) env))
(define (lookup-env [n : Symbol] [env : (Env 'a)]) : 'a
  (type-case (Listof (Binding 'a)) env
    [empty (error 'lookup "unbound identifier")]
    [(cons b rst-env) (cond
                        [(eq? n (bind-name b))
                         (bind-val b)]
                        [else (lookup-env n rst-env)])]))

;; eval --------------------------------------

;; values

(define-type Value
  (numV [n : Number])
  (boolV [b : Boolean])
  (funV [x : Symbol] [e : Exp] [env : (Env Value)])
  (primopV [f : (Value -> Value)]))

;; primitive operations and the initial environment

(define (op-num-num->value [f : (Number Number -> Number)]) : Value 
  (primopV
   (λ (v1)
     (type-case Value v1
       [(numV n1)
        (primopV
         (λ (v2)
           (type-case Value v2
             [(numV n2)
              (numV (f n1 n2))]
             [else
              (error 'eval "type error")])))]
       [else
        (error 'eval "type error")]))))

(define (op-num-bool->value [f : (Number Number -> Boolean)]) : Value 
  (primopV
   (λ (v1)
     (type-case Value v1
       [(numV n1)
        (primopV
         (λ (v2)
           (type-case Value v2
             [(numV n2)
              (boolV (f n1 n2))]
             [else
              (error 'eval "type error")])))]
       [else
        (error 'eval "type error")]))))

(define init-env 
  (foldr (λ (b env) (extend-env env (fst b) (snd b)))
         mt-env 
         (list (pair '+ (op-num-num->value +))
               (pair '- (op-num-num->value -))
               (pair '* (op-num-num->value *))
               (pair '/ (op-num-num->value /))
               (pair '= (op-num-bool->value =))
               (pair '<= (op-num-bool->value <=)))))

;; evaluation function (eval/apply)

(define (eval [e : Exp] [env : (Env Value)]) : Value
  (type-case Exp e
    [(numE n)
     (numV n)]
    [(boolE b)
     (boolV b)]
    [(ifE e0 e1 e2)
     (type-case Value (eval e0 env)
       [(boolV v)
        (if v (eval e1 env) (eval e2 env))]
       [else
        (error 'eval "type error")])]
    [(varE x)
     (lookup-env x env)]
    [(letE x e1 e2)
     (let ([v1 (eval e1 env)])
       (eval e2 (extend-env env x v1)))]
    [(lamE x e0)
     (funV x e0 env)]
    [(appE e1 e2)
     (apply (eval e1 env) (eval e2 env))]))

(define (apply [v1 : Value] [v2 : Value]) : Value
  (type-case Value v1
    [(funV x b env)
     (eval b (extend-env env x v2))]
    [(primopV f)
     (f v2)]
    [else (error 'apply "not a function")]))

(module+ test
  (test (eval (parse `{{lambda {x y} {+ x y}} 1 2}) init-env)
        (numV 3)))
         
;; typecheck --------------------------------------

;; types

(define-type Type
  (numT)
  (boolT)
  (funT [t1 : Type] [t2 : Type])
  (varT [id : Symbol]))

;; fresh type variable generation

(define gen-symbol 
  (local [(define count 0)]
    (lambda ()
      (begin
        (set! count (+ count 1))
        (string->symbol (string-append "a" (to-string count)))))))

(define (gen-var) 
  (varT (gen-symbol)))

; occurs check

(define (occurs [a : Symbol] [t : Type]) : Boolean
  (type-case Type t
    [(numT) #f]
    [(boolT) #f]
    [(funT t1 t2) (or (occurs a t1) (occurs a t2))]
    [(varT b) (eq? a b)]))

(module+ test
  (test (occurs 'a (funT (boolT) (numT)))
        #f)
  (test (occurs 'a (funT (boolT) (varT 'a)))
        #t)
  (test (occurs 'a (funT (boolT) (varT 'b)))
        #f))
  
;; type equations

(define-type TypeEq
  (eqT [l : Type] [r : Type]))

(define-type-alias TypeEqs (Listof TypeEq))

(define teqs-empty : TypeEqs
  empty)

(define (teqs-single [t1 : Type] [t2 : Type]) : TypeEqs
  (list (eqT t1 t2)))

(define (teqs-append [eqs1 : TypeEqs] [eqs2 : TypeEqs]) : TypeEqs
  (append eqs1 eqs2))

(define (teqs-concat [eqss : (Listof TypeEqs)]) : TypeEqs
  (foldr teqs-append teqs-empty eqss))

;; substitution

(define-type TypeSubst
  (substT [a : Symbol] [t : Type]))

(define-type-alias Subst (Listof TypeSubst))

(define subst-empty : Subst
  empty)

(define (subst-single [a : Symbol] [t : Type]) : Subst
  (list (substT a t)))

(define (subst-append [s1 : Subst] [s2 : Subst]) : Subst
  (append s1 s2))

(define (subst-lookup [s : Subst] [a : Symbol]) : (Optionof Type)
  (type-case Subst s
    [empty
     (none)]
    [(cons p ps)
     (if (eq? (substT-a p) a)
         (some (substT-t p))
         (subst-lookup (rest s) a))]))

(define (subst->fun [s : Subst]) : (Symbol -> Type)
  (lambda (a)
    (type-case (Optionof Type) (subst-lookup s a)
      [(none)
       (varT a)]
      [(some t)
       t])))

(define (substitute [t : Type] [s : Subst]) : Type
  (type-case Type t
    [(numT)
     (numT)]
    [(boolT)
     (boolT)]
    [(funT t1 t2)
     (funT (substitute t1 s)
           (substitute t2 s))]
    [(varT a)
     ((subst->fun s) a)]))

(module+ test
  (test (substitute (funT (varT 'a) (varT 'b))
                    subst-empty)
        (funT (varT 'a) (varT 'b)))
  (test (substitute (funT (varT 'a) (varT 'b))
                    (list (substT 'a (boolT)) (substT 'b (numT))))
        (funT (boolT) (numT)))
  (test (substitute (funT (varT 'a) (varT 'b))
                    (list (substT 'a (varT 'b)) (substT 'b (numT))))
        (funT (varT 'b) (numT))))

(define (substitute-teqs [s : Subst] [eqs : TypeEqs]) : TypeEqs
  (map (lambda (p)
         (eqT (substitute (eqT-l p) s)
              (substitute (eqT-r p) s)))
       eqs))

(define (substitute-subst [s1 : Subst] [s2 : Subst]) : Subst
  (map (lambda (p)
         (substT (substT-a p) (substitute (substT-t p) s1)))
       s2))

(module+ test
  (test (substitute-subst (list (substT 'c (boolT))) (list (substT 'a (varT 'c))))
        (list (substT 'a (boolT))))
  (test (substitute-subst (list (substT 'a (boolT))) (list (substT 'a (varT 'c))))
        (list (substT 'a (varT 'c)))))

; substitution composition
; (substitute t (subst-compose s1 s2)) = (substitute (substitute t s1) s2)
 
(define (subst-compose [s1 : Subst] [s2 : Subst]) : Subst
  (subst-append (substitute-subst s2 s1) s2))

(module+ test
  (define s1 (list (substT 'a (varT 'c))))
  (define s2 (list (substT 'c (boolT))))
  (define s3 (list (substT 'a (boolT))))
  (test (subst-compose s1 s2)
        (list (substT 'a (boolT)) (substT 'c (boolT))))
  (test (subst-compose s1 s3)
        (list (substT 'a (varT 'c)) (substT 'a (boolT))))
  (test (substitute (varT 'a) (subst-compose s1 s2))
        (substitute (substitute (varT 'a) s1) s2))
  (test (substitute (varT 'a) (subst-compose s1 s3))
        (substitute (substitute (varT 'a) s1) s3)))

;; unification

(define (unify [eqs : TypeEqs]) : (Optionof Subst)
  (local
    [(define (iter s eqs)
       (type-case TypeEqs eqs
         [empty
          (some s)]
         [(cons eq eqs1)
          (let ([eql (eqT-l eq)]
                [eqr (eqT-r eq)])
            (cond
              ; (varT a) = (varT a) 
              [(and (varT? eql)
                    (varT? eqr)
                    (eq? (varT-id eql) (varT-id eqr)))
               (iter s eqs1)]
              ; (numT) = (numT)
              [(and (numT? eql)
                    (numT? eqr))
               (iter s eqs1)]
              ; (boolT) = (boolT)
              [(and (boolT? eql)
                    (boolT? eqr))
               (iter s eqs1)]
              ; (funT tl1 tl2) = (funT tr1 tr2)  
              [(and (funT? eql)
                    (funT? eqr))
               (iter s (teqs-concat
                        (list (teqs-single (funT-t1 eql) (funT-t1 eqr))
                              (teqs-single (funT-t2 eql) (funT-t2 eqr))
                              eqs1)))]
              ; (varT a) = t
              [(varT? eql)
               (if (occurs (varT-id eql) eqr)
                   (none)
                   (let ([s1 (subst-single (varT-id eql) eqr)])
                     (iter (subst-compose s s1)
                           (substitute-teqs s1 eqs1))))]
              ; t = (varT a)
              [(varT? eqr)
               (iter s (teqs-concat (list (teqs-single eqr eql) eqs1)))]
              ; (numT) = (boolT) or (numT) = (funT t1 t2) or ... 
              [else
               (none)]))]))]
    (iter subst-empty eqs)))

(module+ test
  (test (unify (teqs-single (numT) (varT 'a)))
        (some (list (substT 'a (numT)))))
  (test (unify (teqs-single (funT (numT) (varT 'b)) (varT 'a)))
        (some (list (substT 'a (funT (numT) (varT 'b))))))
  (test (unify (teqs-single (funT (numT) (varT 'b)) (varT 'b)))
        (none))
  (test (unify (teqs-single (funT (numT) (varT 'b))
                            (funT (varT 'a) (varT 'a))))
        (some (list (substT 'a (numT))
                    (substT 'b (numT)))))
  (test (unify (teqs-single (funT (varT 'b) (numT))
                            (funT (varT 'a) (varT 'a))))
        (some (list (substT 'b (numT))
                    (substT 'a (numT)))))
  (test (unify (teqs-single (funT (varT 'a) (varT 'b))
                            (funT (varT 'c) (varT 'c))))
        (some (list (substT 'a (varT 'c))
                    (substT 'b (varT 'c)))))
  (test (unify (teqs-single (funT (varT 'a) (varT 'b))
                            (funT (varT 'b) (varT 'c))))
        (some (list (substT 'a (varT 'c))
                    (substT 'b (varT 'c))))))

;; actual typechecking

(define-type TypecheckResult
  (typecheckR [tp : Type] [eqs : TypeEqs]))

(define (funT-curry t1 t2 t3)
  (funT t1 (funT t2 t3)))

(define op-num-num-type 
  (funT-curry (numT) (numT) (numT)))

(define op-num-bool-type 
  (funT-curry (numT) (numT) (boolT)))
  
(define init-type-env 
  (foldr (λ (b env) (extend-env env (fst b) (snd b)))
         mt-env 
         (list (pair '+ op-num-num-type)
               (pair '- op-num-num-type)
               (pair '* op-num-num-type)
               (pair '/ op-num-num-type)
               (pair '= op-num-bool-type)
               (pair '<= op-num-bool-type))))

(define (typecheck-env [e : Exp] [env : (Env Type)]) : TypecheckResult
  (type-case Exp e
    [(numE n)
     (typecheckR (numT) teqs-empty)]
    [(boolE b)
     (typecheckR (boolT) teqs-empty)]
    [(ifE e0 e1 e2)
     (let* ([r0 (typecheck-env e0 env)]
            [r1 (typecheck-env e1 env)]
            [r2 (typecheck-env e2 env)])
       (typecheckR (typecheckR-tp r1)
                   (teqs-concat
                    (list (teqs-single (typecheckR-tp r0)
                                       (boolT))
                          (teqs-single (typecheckR-tp r1)
                                       (typecheckR-tp r2))
                          (typecheckR-eqs r0)
                          (typecheckR-eqs r1)
                          (typecheckR-eqs r2)))))]
    [(varE x)
     (typecheckR (lookup-env x env) teqs-empty)]
    [(letE x e1 e2)
     (let* ([r1 (typecheck-env e1 env)]
            [r2 (typecheck-env e2 (extend-env env x (typecheckR-tp r1)))])
       (typecheckR (typecheckR-tp r2)
                   (teqs-append (typecheckR-eqs r1)
                                (typecheckR-eqs r2))))]
    
    [(lamE x e0)
     (let* ([arg-t (gen-var)]
            [r (typecheck-env e0 (extend-env env x arg-t))]
            [res-t (typecheckR-tp r)])
       (typecheckR (funT arg-t res-t)
                   (typecheckR-eqs r)))]
    
    [(appE e1 e2)
     (let* ([r1 (typecheck-env e1 env)]
            [r2 (typecheck-env e2 env)]
            [arg-t (typecheckR-tp r2)]
            [res-t (gen-var)])
       (typecheckR res-t
                   (teqs-concat
                    (list (teqs-single (typecheckR-tp r1)
                                       (funT arg-t res-t))
                          (typecheckR-eqs r1)
                          (typecheckR-eqs r2)))))]))

#|
(typecheck-env (parse `{{lambda {x} x} 42}) init-type-env)
(typecheck-env (parse `{lambda {x} {x x}}) init-type-env)
|#

(define (typecheck [e : Exp]) : Type
  (let [(r (typecheck-env e init-type-env))]
    (type-case (Optionof Subst) (unify (typecheckR-eqs r))
      [(none)
       (error 'typecheck "type error")]
      [(some s)
       (substitute (typecheckR-tp r) s)])))

(define (run [s : S-Exp]) : (Type * Value)
  (let ([e (parse s)])
    (pair (typecheck e)
          (eval e init-env))))

(module+ test
  (test (typecheck (parse `42))
        (numT))
  (test (typecheck (parse `{if {= {+ 2 2} 4} 42 69}))
        (numT))
  (test (typecheck (parse `{{lambda {x} {<= x 69}} 42}))
        (boolT))
  (test/exn (typecheck (parse `{{lambda {x} {+ x true}} 2}))
            "type error")
  (test (typecheck (parse `{let k {lambda {x y} x}
                             {{if true k k} 1 {lambda {x} x}}}))
        (numT))
  (test/exn (typecheck (parse `{let k {lambda {x y} x}
                                 {{if true k k} 1 {lambda {x} {x x}}}}))
            "type error"))
