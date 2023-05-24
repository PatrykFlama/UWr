#lang racket

(define ifCond #t)
(define ifTrue "True")
(define ifFalse "False")

(or (and ifCond ifTrue) ifFalse)
