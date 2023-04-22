#lang racket

;TODO provide

; ========DLIST========
(struct del
    (prev val next) #:mutable)

(define (del-empty? xs) (and
    (null? (del-prev xs))
    (null? (del-next xs))))

(define (del-last? xs) (null? (del-next xs)))

; =======DEQUE========
(struct mdeque
    ([front #:mutable]
     [back  #:mutable]))
; front is null <=> back is null

(define (mdeque-empty? dq)
    (null? (mdeque-front dq)))
(define (nonempty-mdeque? dq) (and
    (mdeque? dq)
    (not (empty-mdeque dq))))

(define (empty-mdeque) (mdeque null null))

(define (display-mdeque dq) (mdeque-front dq))


(define (mdeque-push-front dq x)
    (define p (mcons x (mdeque-front dq)))
    (if (mdeque-empty? dq)
        (set-mdeque-back! dq p)
        (void))
    (set-mdeque-front! dq p))
    
(define (mdeque-push-back dq x)
    (define p (mcons x null))
    (if (mdeque-empty? dq)
        (set-mdeque-front! dq p)
        (set-mcdr! (mdeque-back dq) p))
    (set-mdeque-back! dq p))

(define/contract (mdeque-pop-front dq)
    (-> nonempty-mdeque? any/c)
    (define p (mdeque-front dq))
    (set-mdeque-front! dq (mcdr p))
    (if (null? (mcdr p))
        (begin
            (set-mdeque-back! dq null)
            (mcar p))
        (mcar p)))

(define/contract (mdeque-pop-back dq)
    (-> nonempty-mdeque? any/c)
    (define p (mdeque-back dq))
    (set-mdeque-front! dq (mcdr p)) ; TODO set last pointer to null
    (if (null? (mcdr p))
        (begin
            (set-mdeque-back! dq null)
            (mcar p))
        (mcar p)))

(define example-mdeque (empty-mdeque))
(mdeque-push-front example-mdeque 3)
(mdeque-push-front example-mdeque 2)
(mdeque-push-front example-mdeque 1)
(mdeque-push-back  example-mdeque 4)
(mdeque-push-back  example-mdeque 5)
