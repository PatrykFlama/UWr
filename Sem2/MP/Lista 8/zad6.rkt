#lang plait

(define-type Exp
    (exp-var    [name : Symbol])
    (exp-number [number : Number])
    (exp-lambda [parameters : (Listof Symbol)] [application : Exp])
    (exp-app    [function : Exp] [parameters : (Listof Exp)])
    (exp-if     [condition : Exp] [true : Exp] [false : Exp])
    (exp-cond   [cases : (Listof (Exp * Exp))])
    (exp-let    [definitions : (Listof (Symbol * Exp))] [body : Exp]))

