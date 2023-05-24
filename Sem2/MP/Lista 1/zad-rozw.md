[(wróć)](../)
# Zadanie 1
```
10
```
wypisuje liczbę 10
```
(+ 5 3 4)
```
wypisze sumę 5+3+4 czyli 12
```
(- 9 1)
```
wypisze różnicę 9-1 czyli 8
```
(/ 6 2)
```
3
```
(+ (* 2 4) (- 4 6) )
```
wpisze 2*4 + (4-6) czyli 6
```
(define a 3)
```
nic nie wypisze, ale przypisze do _a_ wartość 3
```
(define b (+ a 1) )
```
nic nie wypisze, ale przypisze do _b_ wartość _a+1_ czyli 4
```
(+ a b (* a b ) )
```
wypisze a + b + a * b czyli 3+4+3*4 czyli 19
```
(= a b )
```
sprawdzi czy a = b i wypisze wartość boolowską jako wynik - #f
```
(if ( and ( > b a ) (< b (* a b ) ) )
```
sprawdzi czy b > a i czy b < a*b (prawda)
```
    b
```
jeżeli poprzedni warunek był prawdziwy (był) to wypisze wartość przypisaną do b
```
    a )
```
w przeciwnym przypadku wypisze wartość przypisaną do a
```
(cond [(= a 4) 6]
```
sprawdzi czy a=4, jeżeli tak to wypisze 6
```
    [(= b 4) (+ 6 7 a ) ]
```
w przeciwnym wypadku sprawdzi czy b=4, jeżeli tak to wypisze 6+7+a czyli 16
```
    [ else 25])
```
w przeciwnym wypadku wypisze 25
```
(+ 2 (if ( > b a ) b a ) )
```
jeżeli b > a (jest) to wypisze 2+b czyli 6, w p.p. wypisze 2+a czyli 5
```
(* ( cond [( > a b ) a ]
```
jeżeli a > b to zwróci iloczyn a * (a+1) czyli 12
```
    [(< a b ) b ]
```
w p.p. jeżeli a < b to zwróci iloczyn b * (a+1) czyli 16
```
    [ else -1])
```
w p.p. wypisze -1
```
(+ a 1) )
```

# Zadanie 2
```c
(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5)))) (* 3 (- 6 2) (- 2 7)))
```

# Zadanie 3
```
(* (+ 2 2) 5) 
```
20
```
(* (+ 2 2) (5) ) 
```
błąd - 5 nie jest procedurą więc nie możemy się do niej odnieść/wywołać jej
```
(*(+(2 2) 5) ) 
```
(2 2) racket bierze pierwszy element w nawiasie i stara się go wywołać, ale nie może bo nie jest to coś co może wywołać - jest to zwykła liczba
```
(*(+ 2 
2) 5) 
```
20
```
(5 * 4) 
```
5 nie jest procedurą
```
(5 * (2 + 2) ) 
```
5 nie jest procedurą
```
((+ 2 3) ) 
```
(+ 2 3) zwraca 5, ale 5 nie można wywołać bo nie jest procedurą
```
+ 
```
zwróci wartość + czyli #<procedure:+> - procedurę operacji dodawania
```
( define + (* 2 3) ) 
+ 
```
przypisze do znaku + wartość 6, następnie wypisze wartość + czyli 6
```
(* 2 +) 
```
12
```
( define ( five ) 5) 
```
five będzie procedurą która zwraca 5
```
( define four 4) 
```
przypisze do four 4
```
( five ) 
```
zwróci wartośc procedury five czyli 5
```
four 
```
zwróci wartość four czyli 4
```
five 
```
do five jest przypisana procedura zwracająca 5, więc wypisze jej 'nazwę' #<procedure:five>
```
( four ) 
```
do four przypisana jest liczba 4, więc nie można jej wywołać - błąd

# Zadanie 4
```
#lang racket
(require rackunit)

(define (fun arg1 arg2 arg3) (
    cond [{or (> arg1 arg2 arg3) (> arg2 arg1 arg3)} 
          {+ (* arg1 arg1) (* arg2 arg2)}]
         [{or (> arg1 arg3 arg2) (> arg3 arg1 arg2)} 
          {+ (* arg1 arg1) (* arg3 arg3)}]
         [else {+ (* arg2 arg2) (* arg3 arg3)}]
))

(check-equal? (fun 1 2 3) 13)
(check-equal? (fun 3 2 5) 34)
(check-equal? (fun 7 8 0) 113)
```

# Zadanie 5
```
(define ( a-plus-abs-b a b )
    ((if ( > b 0) + -) a b ) )
```
procedura przypisuje do funkcji nazwanej a-plus-abs-b i przyjmującej argumenty a i b a+b jeżeli b > 0 lub a-b jeżeli b < 0\
dzięki temu znak liczby b nie ma znaczenie - i tak będzie traktowana jako dodatnia i dodawana do a

# Zadanie 6
```c
(or (and ifCond ifTrue) ifFalse)
```

# Zadanie 7
Osobno w kodzie
