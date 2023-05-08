#lang racket
(require rackunit)

(define morse-code-list
  '((#\A . ".-")   (#\B . "-...") (#\C . "-.-.") (#\D . "-..")
    (#\E . ".")    (#\F . "..-.") (#\G . "--.")  (#\H . "....")
    (#\I . "..")   (#\J . ".---") (#\K . "-.-")  (#\L . ".-..")
    (#\M . "--")   (#\N . "-.")   (#\O . "---")  (#\P . ".--.")
    (#\Q . "--.-") (#\R . ".-.")  (#\S . "...")  (#\T . "-")
    (#\U . "..-")  (#\V . "...-") (#\W . ".--")  (#\X . "-..-")
    (#\Y . "-.--") (#\Z . "--..")
    (#\a . ".-")   (#\b . "-...") (#\c . "-.-.") (#\d . "-..")
    (#\e . ".")    (#\f . "..-.") (#\g . "--.")  (#\h . "....")
    (#\i . "..")   (#\j . ".---") (#\k . "-.-")  (#\l . ".-..")
    (#\m . "--")   (#\n . "-.")   (#\o . "---")  (#\p . ".--.")
    (#\q . "--.-") (#\r . ".-.")  (#\s . "...")  (#\t . "-")
    (#\u . "..-")  (#\v . "...-") (#\w . ".--")  (#\x . "-..-")
    (#\y . "-.--") (#\z . "--..")
    (#\0 . "-----") (#\1 . ".----") (#\2 . "..---") (#\3 . "...--")
    (#\4 . "....-") (#\5 . ".....") (#\6 . "-....") (#\7 . "--...")
    (#\8 . "---..") (#\9 . "----.")
    (#\, . "--..--") (#\; . "-.-.-.") (#\? . "..--..")
    (#\: . "---...") (#\' . ".----.") (#\- . "-....-") 
    (#\/ . "-..-.")  (#\space . " ")  (#\. . ".-.-.-")
    (#\+ . ".-.-.")  (#\@ . ".--.-.") (#\= . "-...-")))

(define (add-char c str)
    (list->string (cons c (string->list str))))

; zad4
(define (morse-code str)
    (define (_code-char c code)
        (cond
            [(null? code) (error "morse-code: unknown character" c)]
            [(char=? c (caar code)) (cdar code)]
            [else (_code-char c (cdr code))]))
    (cond
        [(equal? str "") ""]
        [(char-whitespace? (string-ref str 0)) (add-char #\space (morse-code (substring str 1)))]
        [else (string-join (list 
            (_code-char (string-ref str 0) morse-code-list)
            (morse-code (substring str 1))))]))

; zad5
(define (morse-decode str)
    (define (_morse-decode strs)
        (define (_decode-char c code)
            (cond
                [(null? code) (error "morse-decode: unknown sequence" c)]
                [(equal? c (cdar code)) (caar code)]
                [else (_decode-char c (cdr code))]))
        (cond
            [(empty? strs) ""]
            [(equal? (first strs) "") (add-char #\space (_morse-decode (rest strs)))]
            [else (add-char 
                (_decode-char (first strs) morse-code-list)
                (_morse-decode (rest strs)))]))
    (_morse-decode (string-split str " ")))

(check-equal? "METODY PROGRAMOWANIA" (morse-decode (morse-code "Metody Programowania")))
