#lang racket
(require rackunit)

(define (fun arg1 arg2 arg3) (
    cond [{or (> arg1 arg2 arg3) (>= arg2 arg1 arg3)} 
          {+ (* arg1 arg1) (* arg2 arg2)}]
         [{or (> arg1 arg3 arg2) (>= arg3 arg1 arg2)} 
          {+ (* arg1 arg1) (* arg3 arg3)}]
         [else {+ (* arg2 arg2) (* arg3 arg3)}]
))

(check-equal? (fun 1 2 3) 13)
(check-equal? (fun 3 2 5) 34)
(check-equal? (fun 7 8 0) 113)
