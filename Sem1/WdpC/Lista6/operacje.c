#include "operacje.h"

bool czy_palindrom(char *begin){
    char *end = begin;
    while(*end != '\0') end++;

    char *l = begin, *r = end-1;
    while(l < r){
        if(*l != *r){
            return 0;
        }
        l++, r--;
    }

    return 1;
}

void wypisz_rewers(char *begin){
    char *end = begin;
    while(*end != '\0') end++;

    end--;
    while(begin <= end){
        putchar(*end);
        end--;
    }
}
