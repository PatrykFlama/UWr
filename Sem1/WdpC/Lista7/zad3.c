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

/* --- MISC --- */
#define nl putchar('\n')
// #endregion
#define N 1005
#define K 1005
int cnt[N][30];
char s[N][K];


int main(){
    int n, k; 
    scanf("%d%d", &n, &k);
    FOR(i, n){
        FOR(j, k){
            while(s[i][j] < 'a' || s[i][j] > 'z') s[i][j] = getchar();
            cnt[j][s[i][j]-'a']++;
        }
    }

    int max_ptr = 0, maxx = 0;
    FOR(i, n){
        int sum = 0;
        FOR(j, k){
            if(cnt[j][s[i][j]-'a'] == 1) sum++;
        }

        if(sum > maxx){
            maxx = sum;
            max_ptr = i;
        }
    }

    FOR(j, k) putchar(s[max_ptr][j]);
    nl;
    printf("%d\n", maxx);
}
