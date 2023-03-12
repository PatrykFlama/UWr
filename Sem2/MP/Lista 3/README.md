# Lista 3
## Zadanie 1
```
'((car (a . b)) (* 2))
(list `(car (a . b)) `(* 2))
```
```
`(,( car '(a . b)) ,(* 2))
(list (car '(a . b)) (* 2))
```
```
'((+ 1 2 3) ( cons ) ( cons a b))
(list '(+ 1 2 3) '(cons) '(cons a b))
```

## Zadanie 2
```
(define (mult x acc) (* x acc))
(define (mult-list xs)
    (foldl mult 1 xs))
```
Dla pustej zwraca 1 - element neutralny mnożenia

## Zadanie 3
```
(( lambda (x y) (+ x (* x y))) 1 2)
```
Zwraca 1 + (1 * 2) czyli **3**
```
(( lambda (x) x) ( lambda (x) x))
```
(lambda (x) x) jest funkcją identyczności - procedurą (bo nie dostała argumentu i nie została wywołana)\
następnie podajemy jej argument, będący procedurą i wywołujemy, czyli otrzymujemy procedurę (lambda (x) x)
```
(( lambda (x) (x x)) ( lambda (x) x))
```
pierwsza lambda bierze x, a następnie go wywołuje wraz z argumentem x\
wywołana funkcja jest funkcją identyczności - w efekcie otrzymujemy procedurę
```
(( lambda (x) (x x)) (lambda (x) (x x)))
```
pierwsza lambda bierze x oraz go wywołuje z argumentem x\
jako x otrzymujemy taką samą funkcję\
w efekcie otrzymujemy funkcję wywołującą siebie samą w nieskończoność

## Zadanie 4
```
(define (my-compose f g)
    (lambda (x) (f (g x))))
```
```
(( my-compose square inc ) 5)
```
zwraca pierwiastek z (5+1) czyli 2.449489742783178
```
(( my-compose inc square ) 5)
```
zwraca (pierwiastek z 5) + 1 czyli 3.23606797749979

## Zadanie 5
```
(define (negatives n)
    (build-list n (lambda (x) (- (* x -1) 1))))
```
```
(define (reciprocals n)
    (build-list n (lambda (x) (/ 1 (+ x 1)))))
```
```
(define (evens n)
    (build-list n (lambda (x) (* x 2))))
```
```
(define (identityM n)
    (build-list n
        (lambda (col) (build-list n
            (lambda (row)
                (if (= col row) 1 0))))))
```

## Zadanie 7
```
(define ( foldr-reverse xs )
    (foldr ( lambda (y ys ) ( append ys ( list y))) null xs ))

(length (foldr-reverse (build-list 10000 identity)))
```
Dla listy o długości n funkcja ta tworzy _XXX_ consów, z czego _YYY_ jest zbędnych.

## Zadanie 8