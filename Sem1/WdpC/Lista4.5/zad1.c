// Patryk Flama

// #region
/* --- LIBRARIES --- */
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

/* --- SUPERFOR --- */
#define GET_MACRO_FOR(_1, _2, _3, _4, NAME, ...) NAME
#define _FOR3(i, a, n, inc) for (unsigned int i = (a); (inc) > 0 ? i < (n) : i >= (n); i += (inc))
#define _FOR2(i, a, n) _FOR3(i, a, n, 1)
#define _FOR1(i, n) _FOR2(i, 0, n)
#define _FOR0(n) _FOR1(i, n)
#define FOR(...) GET_MACRO_FOR(__VA_ARGS__, _FOR3, _FOR2, _FOR1, _FOR0)(__VA_ARGS__)    //? (/name/, //from//, to, ///inc///)

/* --- MISC --- */
#define fill(tab, len, what) FOR(fill_template_iterator, len) tab[fill_template_iterator] = what;

/* --- C BASIC CARE PACKAGE --- */
#define max(a, b) (a > b ? a : b)
#define min(a, b) (a < b ? a : b)
// #endregion

typedef unsigned int Data;
//? [3] dzien tyg; [5] dzien mies; [4] mies; [4] rok
int potegi[5] = {1, 3, 3+5, 3+5+4, 3+5+4+4};
char dniTygodnia[][7] = {"pon", "wt", "sr", "czw", "pt", "sob", "niedz"};
int maxDzienMies[12] = {31, 28, 31, 30, 31, 30, 31, 30, 31, 30, 31, 30};
char nazwaMies[][12] = {"sty", "lut", "marz", "kwiec", "maj", "czer", "lip", "sier", "wrze", "paz", "lis", "gru"};
char nazwaRoku[][12] = {"malpy", "koguta", "swini", "szczura", "tygrysa", "krolika", "smoka", "weza", "konia", "owcy", "kozy", "psa"};
const int max_dni_tyg = 7, max_mies = 12, max_lat = 12;

void wypiszMaske(Data maska, int n){
    for(Data i = 1; i < (1<<n); i<<=1) {
        if(maska&i) printf("1");
        else printf("0");
    }
    printf("\n");
}

void wypiszDate(Data data){
    int maska = 0;
    for(Data i = 1; i < (1<<potegi[1]); i = i<<1) maska |= i;
    maska &= data;
    printf("%s, ", dniTygodnia[maska-1]);

    maska = 0;
    for(Data i = 1; i < (1<<potegi[2]); i = i<<1) maska |= i;
    maska &= data;
    maska >>= potegi[1];
    printf("%u ", maska);

    maska = 0;
    for(Data i = 1; i < (1<<potegi[3]); i = i<<1) maska |= i;
    maska &= data;
    maska >>= potegi[2];
    printf("%s, ", nazwaMies[maska-1]);

    maska = 0;
    for(Data i = 1; i < (1<<potegi[4]); i = i<<1) maska |= i;
    maska &= data;
    maska >>= potegi[3];
    printf("rok %s(%d)\n", nazwaRoku[maska-1], maska);
}

int BladDaty(Data data){
    // return 1 -> blad
    if(data > (1 << potegi[4])) return 1;

    int maska_mies = 0; 
    int maska_dzien = 0;
    int maska_dzien_tyg = 0;
    int maska_rok = 0;

    for(Data i = 1; i < (1<<potegi[1]); i = i<<1) maska_dzien_tyg |= i;
    maska_dzien_tyg &= data;

    for(Data i = 1; i < (1<<potegi[2]); i = i<<1) maska_dzien |= i;
    maska_dzien &= data;
    maska_dzien >>= potegi[1];

    for(Data i = 1; i < (1<<potegi[3]); i = i<<1) maska_mies |= i;
    maska_mies &= data;
    maska_mies >>= potegi[2];

    for(Data i = 1; i < (1<<potegi[4]); i = i<<1) maska_rok |= i;
    maska_rok &= data;
    maska_rok >>= potegi[3];

    if(maxDzienMies[maska_mies-1] < maska_dzien) return 1;
    if(maska_dzien_tyg <= 0 || maska_mies <= 0 || maska_dzien <= 0 || maska_rok <= 0) return 1;

    return 0;
}


int main(){
    Data data;
    scanf("%u", &data);
    
    while(data != 0 || feof(stdin)){
        if(BladDaty(data)) printf("Bledna data!!!\n");
        else wypiszDate(data);

        scanf("%u", &data);
    }
}