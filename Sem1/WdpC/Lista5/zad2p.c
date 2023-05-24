//Patryk Flama

// #region
/* --- LIBRARIES --- */
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <stddef.h>

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
bool to_file = false;
FILE *output_file;

#define N (int)(1e7+5)
bool is_prime[N];


int to_int(char *string){
    int ptr = 0, num = 0;
    while(string[ptr] != '\0'){
        num *= 10;
        num += string[ptr++]-'0';
    }

    return num;
}

void calc_prime_numbers(int max_n){
    is_prime[0] = 1, is_prime[1] = 1;
    FOR(i, 2, max_n+10){
        if(!is_prime[i]){
            for(int j = i+i; j < max_n+10; j += i){
                is_prime[j] = 1;
            }
        }
    }
}

void initialize_file(int n){
    fprintf(output_file, "P3\n%d %d\n255\n", n, n);
}

void print_pixel(int n){
    if(!is_prime[n]){
        // printf("%d ", n);
        fprintf(output_file, "255 255 0 ");    // yellow
    } else{
        fprintf(output_file, "0 0 255 ");   // blue
    }
}

void wypisz_wiersz(int row, int size){
    if(size%2 == 0){        // even
        if(row == 1){
            FOR(size) {if(to_file) print_pixel(size*size-3*size+3+i); else printf("%4d", size*size-3*size+3+i);};
        } else if(row == size){
            FOR(size) {if(to_file) print_pixel(size*size-i); else printf("%4d", size*size-i);};
        } else if(size > row){
            {if(to_file) print_pixel(size*size-3*size+3-row+1); else printf("%4d", size*size-3*size+3-row+1);};
            wypisz_wiersz(row-1, size-2);
            {if(to_file) print_pixel(size*size-size+1-size+row); else printf("%4d", size*size-size+1-size+row);};
        }
    } else{                 // odd
        if(row == 1){
            FOR(size) {if(to_file) print_pixel(size*size-size+1+i); else printf("%4d", size*size-size+1+i);};
        } else if(row == size){
            FOR(size) {if(to_file) print_pixel(size*size-2*size+2-i); else printf("%4d", size*size-2*size+2-i);};
        } else if(row < size){
            {if(to_file) print_pixel(size*size-size+1-row+1); else printf("%4d", size*size-size+1-row+1);};
            wypisz_wiersz(row-1, size-1);
        }
    }
}


int main(int argc, char** argv){
    int n = 12;
    if(argc > 1) n = to_int(argv[1]);
    
    if(argc > 2){
        to_file = true;
        output_file = fopen(argv[2], "w");
        calc_prime_numbers(n*n+3);
        initialize_file(n);
    }

    FOR(row, 1, n+1){
        wypisz_wiersz(row, n);
        {if(to_file) fprintf(output_file, "\n"); else printf("\n");}
    }

    if(to_file) fclose(output_file);
}
