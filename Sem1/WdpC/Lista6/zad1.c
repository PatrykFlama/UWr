// #region
/* --- LIBRARIES --- */
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <ctype.h>
#include <stddef.h>

/* --- SUPERFOR --- */
#define GET_MACRO_FOR(_1, _2, _3, _4, NAME, ...) NAME
#define _FOR3(i, a, n, inc) for (unsigned int i = (a); (inc) > 0 ? i < (n) : i >= (n); i += (inc))
#define _FOR2(i, a, n) _FOR3(i, a, n, 1)
#define _FOR1(i, n) _FOR2(i, 0, n)
#define _FOR0(n) _FOR1(i, n)
#define FOR(...) GET_MACRO_FOR(__VA_ARGS__, _FOR3, _FOR2, _FOR1, _FOR0)(__VA_ARGS__)    //? (/name/, //from//, to, ///inc///)

/* --- VARS --- */
#define ll long long int

/* --- C BASIC CARE PACKAGE --- */
#define max(a, b) (a > b ? a : b)
#define min(a, b) (a < b ? a : b)
// #endregion

#define L 1000


bool czy_palindrom(char *begin, char *end){
    char *l = begin, *r = end-1;
    while(l < r){
        if(*l != *r){
            return 0;
        }
        l++, r--;
    }

    return 1;
}

void wypisz_rewers(char *begin, char*end){
    end--;
    while(begin <= end){
        putchar(*end);
        end--;
    }
}


int main(){
    char *string = (char*)malloc(L*(sizeof(char)));
    
    char *end = string; 
    scanf("%c", end);
    while(*end != '\n' && *end != ' '){
        end++;
        scanf("%c", &*&*&*&*&*&*&*&*&*&*&*&*&*&*&*&*&*&*&*&*&*end);
    }
    *(end) = '\0';

    if(czy_palindrom(string, end)){
        printf("PALINDROM: %s", string);
    } else{
        printf("REWERS: ");
        wypisz_rewers(string, end);
    }

    free(string);
}
