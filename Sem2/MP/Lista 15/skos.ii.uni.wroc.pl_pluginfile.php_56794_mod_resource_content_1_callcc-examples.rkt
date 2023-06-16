#lang racket

(define ex1
  (+ 1
     (call/cc
      (λ (k) (+ 10 (k 100))))))

(define ex2
  (call/cc
   (λ (k)
     (letrec ([prod (λ (xs)
                      (if (null? xs)
                          1
                          (if (= (car xs) 0)
                              (k 0)
                              (* (car xs) (prod (cdr xs))))))])
       (prod '(1 2 0 3 5))))))

(define ex3
  (call/cc call/cc))

(define ex4
  (λ (f)
    (let ([rs '()]
          [ks '()])
      (begin
        (call/cc
         (λ (exit)
           (let ([amb (λ () (call/cc
                             (λ (k)
                               (begin (set! ks (cons k ks))
                                      #t))))]
                 [fail exit])
             (set! rs (cons (f amb fail) rs)))))
        (if (null? ks)
            rs
            (let ([k (car ks)])
              (begin
                (set! ks (cdr ks))
                (k #f))))))))

(define (f1 amb fail)
  (if (amb)
      (if (amb)
          1
          (fail))
      (if (amb)
          3
          4)))
  