// Patryk Flama zadanie 1

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
// #endregion


int main(){
    int zganiezdzenie = 0;
    char c, last = '0';

    printf("0\t");

    while((c = getchar()) != EOF){    
        if(c == '{') zganiezdzenie++;
        else if(c == '}') zganiezdzenie--;

        if(last == '\n') printf("%d:\t", zganiezdzenie);

        printf("%c", c);

        if(zganiezdzenie < 0){
            printf("Nieodpowienia zastosowanie nawiasoawnia!\n");
            return 0;
        }
        last = c;

        c = getchar();
    }

}