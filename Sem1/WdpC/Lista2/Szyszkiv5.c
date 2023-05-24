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
const int L = 105;
#define height 0
#define in_shadow 1

// void print_garden(long long h, long long w, long long day, long long day_phase){
//     printf("_____________\n");
//     if(day_phase != -1) printf("Day: %lld, phase %lld\n", day, day_phase);
//     FOR(i, h) {FOR(j, w){
//         printf("%lld ", garden[i][j][height]);
//     } printf("\n");}
// }

int main() {
    long long garden[L][L][2];   //? [col][row][tree/in_shadow/has_not_grown_for_X_phases]
    long long trees[L*L][2];        //? list of trees [coor_h/coor_w]

    FOR(i, L) FOR(j, L) FOR(k, 2) garden[i][j][k] = 0;
    FOR(i, L*L) FOR(j, 2) trees[i][j] = 0;

    // ----------- READ -----------
    long long w, h, d; // width, height, days
    scanf("%lld%lld%lld", &w, &h, &d);

    long long trees_amt = 0;
    FOR(i, h){
        FOR(j, w){
            char c; scanf(" %c", &c);
            if(c == '.'){           // for no trees in place, leave 0
                garden[i][j][height]=0;
                continue;
            }
            garden[i][j][height] = c-'0';       // if there is a tree, save its value
            if(garden[i][j][height] < 9){       // and save tree location
                trees[trees_amt][0] = i; trees[trees_amt][1] = j;
                trees_amt++;
            }
        }
    }

    // --------------- MAIN ---------------
    long long cones = 0;
    FOR(day, 10){       // simulate up to 10 days
        FOR(phase, 3){      // simulate day phase: 0<-, 1/\, 2->
            FOR(i, trees_amt){      // clear trees in shadow
                garden[trees[i][0]][trees[i][1]][in_shadow] = 0;
            }
            FOR(i, trees_amt){      // tag all trees in shadow - pick a tree
                FOR(j, 1, garden[trees[i][0]][trees[i][1]][height]+1){ // traverse thru tree height
                    if(phase == 0) if(trees[i][1]-j >= 0) garden[trees[i][0]][trees[i][1]-j][in_shadow] = 1;
                    if(phase == 1) if(trees[i][0]-j >= 0) garden[trees[i][0]-j][trees[i][1]][in_shadow] = 1;
                    if(phase == 2) if(trees[i][1]+j < w)  garden[trees[i][0]][trees[i][1]+j][in_shadow] = 1;
                }
            }

            FOR(i, trees_amt){  // for trees not in shadow: grow them or cone them
                if(garden[trees[i][0]][trees[i][1]][in_shadow] == 0){           // for trees not in shadow
                    if(garden[trees[i][0]][trees[i][1]][height] < 9)                // if tree is not mature, grow it
                        garden[trees[i][0]][trees[i][1]][height]++;
                    else cones++;                                                   // if tree is mature, cone it
                }
            }
        }
        
        print_garden(h, w, day, 0);

        d--; if(d == 0) break;
    }

    // --- calcualte cones per day per tree ---
    long long cones_per_day = 0;
    FOR(phase, 3){
        FOR(i, trees_amt){      // clear trees in shadow
            garden[trees[i][0]][trees[i][1]][in_shadow] = 0;
        }
        FOR(i, trees_amt){      // tag all trees in shadow - pick a tree
            FOR(j, 1, garden[trees[i][0]][trees[i][1]][height]+1){ // traverse thru tree height
                if(phase == 0) if(trees[i][1]-j >= 0) garden[trees[i][0]][trees[i][1]-j][in_shadow] = 1;
                if(phase == 1) if(trees[i][0]-j >= 0) garden[trees[i][0]-j][trees[i][1]][in_shadow] = 1;
                if(phase == 2) if(trees[i][1]+j < w)  garden[trees[i][0]][trees[i][1]+j][in_shadow] = 1;
            }
        }

        FOR(i, trees_amt){
            if(garden[trees[i][0]][trees[i][1]][in_shadow] == 0){
                cones_per_day++;
            }
        }
    }

    cones += d*cones_per_day;

    printf("%lld\n", cones);
}
