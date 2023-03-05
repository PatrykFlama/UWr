//Patryk Flama

// #region
/* --- LIBRARIES --- */
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

/* --- SUPERFOR --- */
#define GET_MACRO_FOR(_1, _2, _3, _4, NAME, ...) NAME
#define _FOR3(i, a, n, inc) for (auto i = (a); (inc) > 0 ? i < (n) : i >= (n); i += (inc))
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


int to_int(char *string){
    int ptr = 0, num = 0;
    while(string[ptr] != '\0'){
        num *= 10;
        num += string[ptr++]-'0';
    }

    return num;
}

void wypisz_wiersz(int row, int size){
    if(size%2 == 0){        // even
        if(row == 1){
            FOR(size) printf("%4d", size*size-3*size+3+i);
        } else if(row == size){
            FOR(size) printf("%4d", size*size-i);
        } else if(size > row){
            printf("%4d", size*size-3*size+3-row+1);
            wypisz_wiersz(row-1, size-2);
            printf("%4d", size*size-size+1-size+row);
        }
    } else{                 // odd
        if(row == 1){
            FOR(size) printf("%4d", size*size-size+1+i);
        } else if(row == size){
            FOR(size) printf("%4d", size*size-2*size+2-i);
        } else if(row < size){
            printf("%4d", size*size-size+1-row+1);
            wypisz_wiersz(row-1, size-1);
        }
    }
}


int main(int argc, char** argv){
    int N = 12;
    if(argc > 1) N = to_int(argv[1]);

    FOR(row, 1, N+1){
        wypisz_wiersz(row, N);
        printf("\n");
    }
}