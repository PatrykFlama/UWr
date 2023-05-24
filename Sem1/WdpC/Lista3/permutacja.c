// #region SUPERFOR
#define GET_MACRO(_1, _2, _3, _4, NAME, ...) NAME
#define _FOR3(i, a, n, inc) for (long long i = (a); (inc) > 0 ? i < (n) : i >= (n); i += (inc))
#define _FOR2(i, a, n) _FOR3(i, a, n, 1)
#define _FOR1(i, n) _FOR2(i, 0, n)
#define _FOR0(n) _FOR1(i, n)
#define FOR(...) GET_MACRO(__VA_ARGS__, _FOR3, _FOR2, _FOR1, _FOR0)(__VA_ARGS__)    //? (/name/, //from//, to, ///inc///)
#define cerr if (debug) cout
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
// #endregion */
// const int L = 1e6+5;

int max(int a, int b){
    if(a > b) return a;
    return b;
}

int main(){
    int n; 
    scanf("%d", &n);
    int tab[n+1];
    tab[0] = 0;

    FOR(i, 1, n+1){
        scanf("%d", &tab[i]);
    }

    int maxx = 0;
    FOR(i, 0, n+1){
        if(tab[i] == -1) continue;
        
        int ptr = tab[i];
        int len = 1;
        while(ptr != i){
            len++;
            int temp = ptr;
            ptr = tab[ptr];
            tab[temp] = -1;
        }

        maxx = max(maxx, len);
    }

    printf("%d\n", maxx);
}