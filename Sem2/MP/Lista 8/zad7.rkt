#lang plait

;* ----types----
(define-type Exp
  (exp-var    [name : Symbol])
  (exp-number [number : Number])
  (exp-lambda [parameters : (Listof Symbol)] [application : Exp])
  (exp-app    [function : Exp] [parameters : (Listof Exp)])
  (exp-if     [condition : Exp] [true : Exp] [false : Exp])
  (exp-cond   [cases : (Listof (Exp * Exp))])
  (exp-let    [definitions : (Listof (Symbol * Exp))] [body : Exp]))

;* ----helepr----
(define (parse-Lambda xs)
    (exp-lambda
        (map s-exp->symbol (s-exp->list (first xs))) (parse-Exp (second xs))))

(define (parse-if xs)
    (exp-if 
        (parse-Exp (first xs))
        (parse-Exp (second xs))
        (parse-Exp (third xs))))

(define (make-pair f s)
    (let ([xs (s-exp->list s)])
        (pair (f (first xs)) (parse-Exp (second xs)))))

(define (parse-cond xs)
    (exp-cond
        (map (lambda (x) (make-pair parse-Exp x)) xs)))

(define (parse-Let xs)
    (exp-let
        (map (lambda (x) (make-pair s-exp->symbol x)) (s-exp->list (first xs))) (parse-Exp (second xs))))

(define (parse-App xs)
    (exp-app
        (parse-Exp (first xs))
            (map parse-Exp (rest xs))))
  
;* ----parser----
(define (parse-Exp s)
    (cond 
        [(s-exp-symbol? s) (exp-var (s-exp->symbol s))]
        [(s-exp-number? s) (exp-number (s-exp->number s))]
        [else (let ([xs (s-exp->list s)])
            (cond 
                [(s-exp-symbol? (first xs))
                    (let ([op (s-exp->symbol (first xs))])
                    (cond 
                        [(equal? op 'lambda) (parse-Lambda (rest xs))]
                        [(equal? op 'if)     (parse-if (rest xs))]
                        [(equal? op 'cond)   (parse-cond (rest xs))]
                        [(equal? op 'let)    (parse-Let (rest xs))]
                        [else                (parse-App xs)]))]
                [else (parse-App xs)]))]))

;* ----tests----
(parse-Exp `(lambda (x y z) (if x y z)))
(parse-Exp `(let ((x 1) (y 2)) (+ x y)))
(parse-Exp `(cond ((= x 1) 1) ((= x 2) 2) (else 3)))
(parse-Exp `(if (= x 1) 1 2))
(parse-Exp `(+ 1 2 3))
