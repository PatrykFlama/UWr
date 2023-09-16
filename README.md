# UWr

[\[English\]](README.en.md)

## [Semestr 1](Sem1/)

<details>
    <summary> Plan </summary>
    
```mermaid
gantt
    title Zimowy 22/23
    dateFormat HH
    axisFormat %H:%M

    section Monday
        LDI : 10, 12
        Logika dla informatyków : active, 12, 14

    section Tuesday
        Analiza I : active, 08, 10
        Podstawowy warsztat informatyka: active, 11, 12

    section Wednesday
        Algebra liniowa 1R : active, 09, 11
        Algebra liniowa 1R : 11, 13
        WdpC : 14, 16
        PWI : 16, 17

    section Thursday
        Analiza I : 08, 10
        Wstęp do programowania w C : active, 10, 12
        Algebra liniowa 1R : active, 12, 14

    section Friday
        Analiza I : 08, 10
        Analiza I : active, 10, 12
        MIA : 14, 16
        Metody implementacji algorytmów : active, 16, 17

```
</details>

* Analiza matematyczna I
* Algebra liniowa 1R
* Logika dla informatyków
* [**W**stęp **d**o **p**rogramowania w **C**](Sem1/WdpC/)
* [**M**etody **I**mplementacji **A**lgorytmów](Sem1/MIA/)
* Podstawowy warsztat informatyka
* Język angielski poziom B2

## [Semestr 2](Sem2/)

<details>
    <summary> Plan </summary>

```mermaid
gantt
    title Letni 22/23
    dateFormat HH
    axisFormat %H:%M

    section Monday
        Algebra liniowa 2   : 12, 14 

    section Tuesday
        Algebra liniowa 2   : active, 09, 12
        MP                  : 12, 15
        PARO                : 16, 19

    section Wednesday
        Analiza II          : active, 08, 10
        Sztuczna inteligencja : active, 08, 10
        Metody programowania : active, 10, 13

    section Thursday
        Seminarium Młody Badacz : 08, 10
        Programowanie obiektowe : active, 10, 12
        AI                      : 10, 12
        Analiza II              : active, 12, 14
        PO                      : 12, 14
        Kurs C++                : 14, 16
        Analiza II              : 16, 18
        Kurs C++                : active, 16, 18

    section Friday
        Analiza II              : 08, 10
```
</details>

* Analiza matematyczna II
* Algebra liniowa 2
* [**A**rtificial **I**ntelligence](Sem2/AI)
* [**M**etody **P**rogramowania](Sem2/MP/)
* [**P**rogramowanie **O**biektowe](Sem2/PO/)
* [Kurs języka **C++**](Sem2/CPP/)
* [**P**raktyczne **A**spekty **R**ozwoju **O**programowania](Sem2/PARO/)

## [Semestr 3](Sem3/)

<details open>
    <summary> Planer </summary>

Preferowane (na podstawie zimowego 22/23):  
**MDM**: Stachowiak; Dudek?  
**AN**: Karczewski, Nowak  
**ML**: Adamczyk, Biernacka Stypułkowski, Słupiński, Balcer   
**Kursz rozszerzony Python**: Młotkowski  

Preferowane (na podstawie opinii):  
**MDM**: Stachowiak; Dudek?  
**AN**: Woźny (ciężej, ale dokładniej), Karczewski (luźny, łatwy, gorzej uczy), Nowak (lepiej nie)  
**ML**: Adamczyk, Biernacka Stypułkowski, Słupiński, Balcer   
**Kursz rozszerzony Python**: Młotkowski  
**WEPPO**: Zchla

**Plan ataku:**  
* 1 [ML](https://zapisy.ii.uni.wroc.pl/courses/machine-learning-202324-zimowy) (Adamczyk)                   : 16, 18
* 1 [MDM](https://zapisy.ii.uni.wroc.pl/courses/matematyka-dyskretna-m-202324-zimowy) (Stachowiak)                : 16, 19
* 1 [AN](https://zapisy.ii.uni.wroc.pl/courses/analiza-numeryczna-202324-zimowy) (Woźny)                      : 10, 12
* 1 [PY](https://zapisy.ii.uni.wroc.pl/courses/kurs-rozszerzony-jezyka-python-202324-zimowy) (Młotkowski)                 : 10, 12
* 1 [KPABD](https://zapisy.ii.uni.wroc.pl/courses/kurs-projektowania-aplikacji-z-bazami-danych-202324-zimowy) (Rajba)                   : 18, 20
* 1 [WEPPO](https://zapisy.ii.uni.wroc.pl/courses/wybrane-elementy-praktyki-projektowania-oprogramowania-202324-zimowy) (Wieczorek)               : 14, 16
* 2 AN (Karczewski)                 : 16, 18
* [Ekonomia](https://zapisy.ii.uni.wroc.pl/courses/o-ekonomii-i-gospodarce-inaczej-w-202324-zimowy)                        : active, 14, 16
* 

_active_ => lectures  
_done_ => probably non-existent  
_%_ => strict collision  
_%%_ => prefert to not  
_%%%_ => backup  
_%%%%_ => to analyze  
_%%%%%_ => WEPPO

```mermaid
gantt
    title Zimowy 23/24
    dateFormat HH
    axisFormat %H:%M

    section Monday
        Matematyka Dyskretna M          : active, 14, 17
        Machine Learning                : active, 14, 16
        KPA z bazami danych             : active, 16, 18

        1 ML (Adamczyk)                   : 16, 18
        1 KPABD (Rajba)                   : 18, 20

        Algebra 1                       : active, 12, 14

    section Tuesday
        %%%%% WEPPO                           : active, 10, 12
        Ekonomia                        : active, 14, 16

        %%% 3 ML (Słupiński)                  : 08, 10
        %%% 4 ML (Balcer)                     : 08, 10
        1 AN (Woźny)                      : 10, 12
        AN (Woźny)                      : 14, 16
        %% AN (Nowak)                      : 10, 12
        %% AN (Nowak)                      : 12, 14
        AN (Karczewski)                 : 14, 16
        %%% 2 AN (Karczewski)                 : 16, 18
        %%% 2 KPABD (Abbasi)                  : 18, 20
        %% PY (Balcer)                     : 10, 12
        %% PY (Piróg)                      : 12, 14
        % 1 WEPPO (Zychla)                  : 08, 10
        %%%%% 2 WEPPO (Zychla)                  : 12, 14
        %%%%% WEPPO (Gańczorz)                : 14, 16

        Algebra 1                       : active, 08, 10
        Analiza III (wyk)               : done, 16, 18
        Analiza III                     : done, 14, 16

    section Wednesday
        Analiza numeryczna              : active, 10, 13
        Analiza III                     : active, done, 08, 10

        %%% 2 MDM (Jeż)                       : 14, 17
        %%% 2 ML (Biernacka)                  : 14, 16
        % ML (Stypułkowski)               : 10, 12
        %%% 3 KPABD (Abbasi)                  : 18, 20
        %% PY (Adamczyk)                   : 08, 10
        %%%%% WEPPO (Wieczorek)               : 14, 16

    section Thursday
        Analiza numeryczna (repe)       : active, 12, 14

        1 MDM (Stachowiak)              : 16, 19
        %%% MDM (Dudek)                     : 16, 19
        1 PY (Młotkowski)                 : 10, 12
        %%% 2 PY (Młotkowski)                 : 08, 10
        %% PY (Słupiński)                  : 18, 20
        %%%%% 3 WEPPO (Wieczorek)               : 14, 16

        Analiza III                     : done, 12, 14

    section Friday
        Rozszerzony Python              : active, 10, 12

        %%% 3 PY (Młotkowski)                 : 08, 10
        %%%%% % WEPPO (Gańczorz)                : 10, 12

        Algebra 1                       : 08, 10
```

</details>



