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
#define nl putchar('\n')
// #endregion

bool planks[16][21*21];

int rotated_idx(int row, int col, int n, int times){       // rotate pi/2 cw
    FOR(times) {
        int tmp = row;
        row = n-col-1;
        col = tmp;
    }

    return row+n*col;
}

void calc_holes(bool *holes_exist, bool *holes, int n, int k, int plank){
    if(plank == k){
        int amt = 0;
        FOR(i, n*n){
            if(!holes[i]) amt++;
        }
        holes_exist[amt] = true;
        return;
    }

    bool cp_holes[n*n];

    FOR(rotation, 4){
        FOR(i, n*n) cp_holes[i] = holes[i];
        FOR(row, n){
            FOR(col, n){
                cp_holes[row+col*n] = cp_holes[row+col*n] || planks[plank][rotated_idx(row, col, n, rotation)];
            }
        }
        calc_holes(holes_exist, cp_holes, n, k, plank+1);
    }
}


int main(){
    int n, k; scanf("%d%d", &n, &k);

    FOR(plank, k){
        FOR(row, n){
            FOR(col, n){
                char c; scanf(" %c", &c);
                if(c == '.') planks[plank][row*n+col] = 0;
                else if(c == '#') planks[plank][row*n+col] = 1;
            }
        }
    }

    bool holes_exist[n*n+1], holes[n*n];
    FOR(i, n*n) holes[i] = planks[0][i], holes_exist[i] = false;      // set first set of holes as first plank, as it doesnt have to be rotated

    calc_holes(holes_exist, holes, n, k, 0);

    FOR(i, n*n) if(holes_exist[i]) printf("%d ", i);
    printf("\n");
}
