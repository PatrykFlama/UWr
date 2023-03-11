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
