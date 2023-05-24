// #region
/* --- LIBRARIES --- */
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <stddef.h>
#include <float.h>
#include <math.h>


/* --- SUPERFOR --- */
#define GET_MACRO_FOR(_1, _2, _3, _4, NAME, ...) NAME
#define _FOR3(i, a, n, inc) for (int i = (a); (inc) > 0 ? i < (n) : i >= (n); i += (inc))
#define _FOR2(i, a, n) _FOR3(i, a, n, 1)
#define _FOR1(i, n) _FOR2(i, 0, n)
#define _FOR0(n) _FOR1(i, n)
#define FOR(...) GET_MACRO_FOR(__VA_ARGS__, _FOR3, _FOR2, _FOR1, _FOR0)(__VA_ARGS__)    //? (/name/, //from//, to, ///inc///)

/* --- VARS --- */
#define ll long long int

/* --- C BASIC CARE PACKAGE --- */
#define max(a, b) (a > b ? a : b)
#define min(a, b) (a < b ? a : b)

/* --- MISC --- */
#define nl putchar('\n')
// #endregion
#define P 100000


ll mult(ll a, ll b){
    ll res = 0;

    if(b == 0) return 0;
    if(b == 1) return a;
    
    if(b % 2 == 0){
        res = (mult(a, b/2)*2) % P;
    }
    else{
        res = (mult(a, b/2)*2 + a) % P;
    }
    
    return res;
}


int main(){
    int trashe;
    int t; 
    trashe = scanf(" %d", &t);

    while(t--){
        ll n, x; 
        trashe = scanf(" %lld %lld", &n, &x);
        ll res = 0;
        x %= P;

        FOR(n+1){
            ll a; 
            trashe = scanf(" %lld", &a);
            a %= P;

            if((res > 0 && x > 0) || (res < 0 && x < 0))    res = (mult(llabs(res), abs(x)));
            else                                            res = -1 * (mult(llabs(res), abs(x)));

            res = (res+a)%P;
        }

        res = llabs(res);
        printf("%05lld\n", res);
    }

    trashe++;
}
