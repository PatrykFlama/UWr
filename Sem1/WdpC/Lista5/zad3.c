// #region
/* --- LIBRARIES --- */
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

/* --- SUPERFOR --- */
#define GET_MACRO_FOR(_1, _2, _3, _4, NAME, ...) NAME
#define _FOR3(i, a, n, inc) for (unsigned int i = (a); (inc) > 0 ? i < (n) : i >= (n); i += (inc))
#define _FOR2(i, a, n) _FOR3(i, a, n, 1)
#define _FOR1(i, n) _FOR2(i, 0, n)
#define _FOR0(n) _FOR1(i, n)
#define FOR(...) GET_MACRO_FOR(__VA_ARGS__, _FOR3, _FOR2, _FOR1, _FOR0)(__VA_ARGS__)    //? (/name/, //from//, to, ///inc///)

/* --- MISC --- */
#define fill(tab, len, what) FOR(fill_template_iterator, len) tab[fill_template_iterator] = what;
#define ll unsigned long long int

/* --- C BASIC CARE PACKAGE --- */
#define max(a, b) (a > b ? a : b)
#define min(a, b) (a < b ? a : b)
// #endregion

int main(){
    ll d = 17; 
    ll i = scanf("%llu", &d);

    for(i = sqrt(d*d/3); i > 0; i--){
        for(ll j = sqrt((d*d-i*i)/2); j >= i; j--){
            ll x = sqrt(d*d-i*i-j*j);
            // printf("sqrt dla %lld %lld to: %f\n", i, j, x);
            if(d*d-x*x-i*i-j*j == 0){
                printf("%llu %llu %llu\n", x, j, i);
                return 0;
            }
        }
    }

    printf("BRAK\n");
}
