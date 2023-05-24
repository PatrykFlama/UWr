#lang plait


(define-syntax my-and
  (syntax-rules ()
    [(my-and) #t]
    [(my-and a) a]
    [(my-and a b ...) (if a (my-and b ...) a)]))

(define-syntax my-or
  (syntax-rules ()
    [(my-or) #t]
    [(my-or a) a]
    [(my-or a b ...) (if a a (my-or b ...))]))

(define-syntax my-let ;??
  (syntax-rules ()
    ; [(my-let () a) a]
    [(my-let ([a1 val1] ...) f)
     ((λ (a1 ...) f) val1 ...)]))

(define-syntax my-let* 
  (syntax-rules ()
    [(my-let* () a) a]
    [(my-let* ([a1 val1] [a2 val2] ...) f)
     ((λ (a1) (my-let* ([a2 val2] ...) f)) val1)])) ; tu do a2 mozemy przypisać val1
                                                   ; itd. przy pomocy lambdy

