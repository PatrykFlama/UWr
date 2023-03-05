/*
Patry Flama lista 1 zadanie 2
kompilacja: gcc zad2.c
wejście:
-> 2 liczby całkowite, będące licznikiem i mianownikiem ułamka
-> znak operacji (np. +) oraz typ operacji (zwracanie nowej zmiennej (1) / nadpisywanie drugiego argumentu (2))
np.: +1 jest operacją dodawania, która zwraca nowo utworzoną zmienną
-> 2 liczby całkowite, będące licznikiem i mianownikiem ułamka
*/

#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <stddef.h>
#include <float.h>
#include <math.h>

#define max(a, b) (a > b ? a : b)
#define min(a, b) (a < b ? a : b)
#define nl putchar('\n')


typedef struct Ulamek{
    int licznik, mianownik;
} Ulamek;

void show(Ulamek *u){
    printf("%d/%d", u->licznik, u->mianownik);
}

int wspolny_dzielnik(int a, int b){
    for(int d = 2; d <= min(a, b); d++){
        if(a%d == 0 && b%d == 0) return d;
    }
    return 0;
}

void uprosc(Ulamek *u){
    if(u->mianownik == 0) {
        printf("Nie wolmo dzielic przez 0!\n");
        exit(0);
    }

    int d = wspolny_dzielnik(u->licznik, u->mianownik);
    while(d){
        u->licznik /= d, u->mianownik /= d;
        d = wspolny_dzielnik(u->licznik, u->mianownik);
    }
    if(u->licznik == 0) u->mianownik = 1;
}


Ulamek *nowy_ulamek(int licznik, int mianownik){
    Ulamek *res = (Ulamek*)malloc(sizeof(Ulamek));
    res->licznik = licznik;
    res->mianownik = mianownik;
    
    uprosc(res);
    return res;
}

void przepisz(Ulamek *from, Ulamek *to){
    to->licznik = from->licznik;
    to->mianownik = from->mianownik;
}

// ----operacje wersja 1----
Ulamek *dodaj_1(Ulamek *a, Ulamek *b){
    Ulamek *res = (Ulamek*)malloc(sizeof(Ulamek));
    res->licznik = a->licznik*b->mianownik + b->licznik*a->mianownik;
    res->mianownik = a->mianownik * b->mianownik;

    uprosc(res);
    return res;
}

Ulamek *odejmij_1(Ulamek *a, Ulamek *b){
    Ulamek *res = (Ulamek*)malloc(sizeof(Ulamek));
    res->licznik = a->licznik*b->mianownik - b->licznik*a->mianownik;
    res->mianownik = a->mianownik * b->mianownik;

    uprosc(res);
    return res;
}

Ulamek *pomnoz_1(Ulamek *a, Ulamek *b){
    Ulamek *res = (Ulamek*)malloc(sizeof(Ulamek));
    res->licznik = a->licznik * b->licznik;
    res->mianownik = a->mianownik * b->mianownik;

    uprosc(res);
    return res;
}

Ulamek *podziel_1(Ulamek *a, Ulamek *b){
    Ulamek *res = (Ulamek*)malloc(sizeof(Ulamek));
    res->licznik = b->mianownik * a->licznik;
    res->mianownik = a->mianownik * b->licznik;

    uprosc(res);
    return res;
}

// ----operacje wersja 2----
void dodaj_2(Ulamek *a, Ulamek *b){
    Ulamek *res = (Ulamek*)malloc(sizeof(Ulamek));
    res->licznik = a->licznik*b->mianownik + b->licznik*a->mianownik;
    res->mianownik = a->mianownik * b->mianownik;

    uprosc(res);
    przepisz(res, b);
    free(res);
}

void odejmij_2(Ulamek *a, Ulamek *b){
    Ulamek *res = (Ulamek*)malloc(sizeof(Ulamek));
    res->licznik = a->licznik*b->mianownik - b->licznik*a->mianownik;
    res->mianownik = a->mianownik * b->mianownik;

    uprosc(res);
    przepisz(res, b);
    free(res);
}

void pomnoz_2(Ulamek *a, Ulamek *b){
    Ulamek *res = (Ulamek*)malloc(sizeof(Ulamek));
    res->licznik = a->licznik * b->licznik;
    res->mianownik = a->mianownik * b->mianownik;

    uprosc(res);
    przepisz(res, b);
    free(res);
}

void podziel_2(Ulamek *a, Ulamek *b){
    Ulamek *res = (Ulamek*)malloc(sizeof(Ulamek));
    res->licznik = b->mianownik * a->licznik;
    res->mianownik = a->mianownik * b->licznik;

    uprosc(res);
    przepisz(res, b);
    free(res);
}

// ----input/output----

Ulamek *wczytaj_ulamek(){
    printf("Podaj ulamek (licznik mianownik): ");
    Ulamek *res = (Ulamek*)malloc(sizeof(Ulamek));
    scanf("%d%d", &(res->licznik), &(res->mianownik));
    uprosc(res);
    return res;
}

char *wez_operacje(){
    printf("Wybierz operacje (+ - * /)(1 2): ");
    char *op = (char*)malloc(sizeof(char));
    scanf("%2s", op);
    return op;
}


int main(){
    Ulamek *a = wczytaj_ulamek();
    char *op = wez_operacje();
    Ulamek *b =wczytaj_ulamek();

    printf("Wczytano ulamki:\n");
    show(a);
    nl;
    show(b);
    nl;

    Ulamek *res;

    switch (op[0]){
    case '+':
        switch (op[1]){
        case '1':
            res = dodaj_1(a, b);
            break;

        case '2':
            dodaj_2(a, b);
            res = b;
            break;
        }
        break;
    case '-':
        switch (op[1]){
        case '1':
            res = odejmij_1(a, b);
            break;

        case '2':
            odejmij_2(a, b);
            res = b;
            break;
        }
        break;
    case '*':
        switch (op[1]){
        case '1':
            res = pomnoz_1(a, b);
            break;

        case '2':
            pomnoz_2(a, b);
            res = b;
            break;
        }
        break;
    case '/':
        switch (op[1]){
        case '1':
            res = podziel_1(a, b);
            break;

        case '2':
            podziel_2(a, b);
            res = b;
            break;
        }
        break;
    }

    printf("Wynikowy ulamek: ");
    show(res);
    nl;
}
