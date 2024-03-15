[wróć](../)

# Programowanie Dynamiczne
Idea lekko oparta na _dziel i zwyciężaj_ gdzie optymalizuje ona zbędne powtórzenia obliczenia (rekurencja).  

## Longest Common Subsequence
Dla dwóch ciągów znaleźć najdłuższy wspólny podciąg. Dla uproszczenia reprezentujemy ciągi za pomocą ciągów liter.  

## Problem plecakowy
### Z powtórzeniami
Mamy dany plecak o pojemności `W` oraz `n` przedmiotów, gdzie każdy przedmiot ma wagę `w` oraz wartość `v`. Znaleźć maksymalną wartość przedmiotów, które można umieścić w plecaku.  
Aby rozwiązać ten problem wystarczy stworzyć tablicę w której zapiszemy maksymalną wartość plecaka dla każdej wagi. Iterujemy się po tablicy i w każdym kroku próbujemy dodać do plecaka każdy z przedmiotów, sprawdzając czy poprawia on nasz aktualny wynik.

### Bez powtórzeń
Mamy dany plecak o pojemności `W` oraz `n` przedmiotów, gdzie każdy przedmiot ma wagę `w` oraz wartość `v`. Znaleźć maksymalną wartość przedmiotów, które można umieścić w plecaku.  
Do rozwiązania tego problemu potrzebujemy już dwuwymiarowej tablicy. Pierwszy wymiar odpowiada za wagę przedmiotów w plecaku, a drugi za przedmioty spakowane do plecaka - dla komórki _i, j_ tablica daje max wartość przedmiotów dla max wagi _i_ oraz rozważanych przedmiotów _[1, ..., j]_.  
Iterujemy się po 'tablicy przedmiotów' _i_ oraz (wewnątrz) 'tablicy wag' _j_, według zasady amerykańskich naukowców, w każdym kroku możemy skorzystać z przedmiotu _j_ lub nie. Jeżeli mieści się on w plecaku oraz jego wartość + wartość plecaka zmniejszonego o wagę przedmiotu _j_ jest większa od wartości plecaka bez tego przedmiotu to zapisujemy tą wartość w tablicy.
