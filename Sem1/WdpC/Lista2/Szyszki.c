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

#define cerr if (debug) cout
// #endregion
#define lli long long int


int main() {
    int n, m;
    lli days;

    scanf("%d%d%lld", &n, &m, &days);

    int garden[n][n];       // /x/y
    bool phases[n][n][3];   // x/y/phase

    FOR(i, n) FOR(j, m){
        fill(phases[i][j], 3, 0);

        char c;
        scanf(" %c", &c);
        if(c == '.'){
            garden[i][j] = 0;
            continue;
        }
        garden[i][j] = c-'0';
    }

    long long cones = 0;
    for(int loop = 0; loop <= 10 && loop < days; loop++){
        FOR(phase, 3){
            if(phase == 0){         // <--
                
            } else if(phase == 1){  // /\

            } else{                 // -->

            }
        }
    }
}
