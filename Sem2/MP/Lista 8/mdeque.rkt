#lang racket

(provide 
    mdeque?
    nonempty-mdeque?
    (contract-out
        [mdeque-empty? (-> mdeque? boolean?)]
        [empty-mdeque  (-> mdeque?)]
        [mdeque-push-front (-> mdeque? any/c void?)]
        [mdeque-push-back  (-> mdeque? any/c void?)]
        [mdeque-pop-front  (-> mdeque? any/c)]  
        [mdeque-pop-back   (-> mdeque? any/c)]
        [mdeque-join (-> nonempty-mdeque? nonempty-mdeque? void?)]))

; ========Double Ended List========
(struct del
    (prev val next) #:mutable #:transparent)

; =======DEQUE========
(struct mdeque
    ([front #:mutable]
     [back  #:mutable]))
; front is null <=> back is null

(define (mdeque-empty? dq)
    (null? (mdeque-front dq)))
(define (nonempty-mdeque? dq) (and
    (mdeque? dq)
    (not (mdeque-empty? dq))))

(define (empty-mdeque) (mdeque null null))

(define (display-mdeque dq) 
    (displayln (mdeque-front example-mdeque))
    (displayln (mdeque-back  example-mdeque)))


(define (mdeque-push-front dq x)
    (define p (del null x (mdeque-front dq)))
    (if (mdeque-empty? dq)
        (set-mdeque-back! dq p)
        (set-del-prev! (mdeque-front dq) p))
    (set-mdeque-front! dq p))
    
(define (mdeque-push-back dq x)
    (define p (del (mdeque-back dq) x null))
    (if (mdeque-empty? dq)
        (set-mdeque-front! dq p)
        (set-del-next! (mdeque-back dq) p))
    (set-mdeque-back! dq p))

(define/contract (mdeque-pop-front dq)
    (-> nonempty-mdeque? any/c)
    (define p (mdeque-front dq))
    (set-mdeque-front! dq (del-next p))
    (if (null? (del-next p))
        (begin
            (set-mdeque-back! dq null)
            (del-val p))
        (begin
            (set-del-prev! (mdeque-front dq) null)
            (del-val p))))

(define/contract (mdeque-pop-back dq)
    (-> nonempty-mdeque? any/c)
    (define p (mdeque-back dq))
    (set-mdeque-back! dq (del-prev p))
    (if (null? (del-prev p))
        (begin
            (set-mdeque-back! dq null)
            (del-val p))
        (begin
            (set-del-next! (mdeque-back dq) null)
            (del-val p))))

(define/contract (mdeque-join dq1 dq2)
    (set-del-next! (mdeque-back dq1) (mdeque-front dq2))
    (set-del-prev! (mdeque-front dq2) (mdeque-back dq1))
    (set-mdeque-back!  dq1 (mdeque-back dq2))
    (set-mdeque-front! dq2 null)
    (set-mdeque-back!  dq2 null))

; (define example-mdeque (empty-mdeque))
; (mdeque-push-front example-mdeque 3)
; (mdeque-push-front example-mdeque 2)
; (mdeque-push-front example-mdeque 1)
; (mdeque-push-back  example-mdeque 4)
; (mdeque-push-back  example-mdeque 5)

; (mdeque-front example-mdeque)
; (mdeque-back example-mdeque)
; (mdeque-pop-front example-mdeque)
; (mdeque-pop-back example-mdeque)
; (mdeque-front example-mdeque)
; (mdeque-back example-mdeque)
