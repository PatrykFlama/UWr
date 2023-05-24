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
// #define debug true
const int L = 105;
#define height 0
#define in_shadow 1
#define not_grown 2

    long long garden[L][L][3];   //? [col][row][tree/in_shadow]
void print_garden(long long h, long long w, long long day, long long day_phase){
    printf("_____________\n");
    if(day_phase != -1) printf("Day: %lld, phase %lld\n", day, day_phase);
    FOR(i, h) {FOR(j, w){
        printf("%lld ", garden[i][j][height]);
    } printf("\n");}
}

int main() {
    long long trees[L*L][3];        //? list of trees [height/is_in_shawdow/has_not_grown_for_X_phases]

    long long w, h, d; // width, height, days
    scanf("%lld%lld%lld", &w, &h, &d);

    long long non_grown_trees = 0;
    FOR(i, h){
        FOR(j, w){
            char c; scanf(" %c", &c);
            if(c == '.'){           // for no trees in place, leave 0
                garden[i][j][height]=0;
                continue;
            }
            garden[i][j][height] = c-'0';       // if there is a tree, save its value
            if(garden[i][j][height] < 9){       // and save tree location
                trees[non_grown_trees][0] = i; trees[non_grown_trees][1] = j;
                non_grown_trees++;
            }
        }
    }
    // FOR(i, non_grown_trees) printf("%lld, %lld\n", trees[i][0], trees[i][1]);
    // print_garden(w, h, 0, -1);
    // printf("nongorwn: %lld\n", non_grown_trees);

    long long trees_amt = non_grown_trees, useful_trees = trees_amt;
    long long cones = 0;
    while(d > 0 && non_grown_trees){      // while there are ungrown trees (~max 9 days == 9 loops) or days left
        d--;
        FOR(i, trees_amt){  // clear stuck trees calculation module
            if(garden[trees[i][0]][trees[i][1]][not_grown] < 3) garden[trees[i][0]][trees[i][1]][not_grown] = 0;
        }

        FOR(phase, 3){      // day phase: 0<-, 1/\, 2->
            FOR(i, trees_amt){      // clear trees in shadow
                garden[trees[i][0]][trees[i][1]][in_shadow] = 0;
            }
            FOR(i, trees_amt){
                FOR(j, 1, garden[trees[i][0]][trees[i][1]][height]+1){       // tag all trees in shadow
                    if(phase == 0) if(trees[i][1]-j >= 0) /*if(garden[trees[i][0]][trees[i][1]-j][height] != 0)*/ garden[trees[i][0]][trees[i][1]-j][in_shadow] = 1;
                    if(phase == 1) if(trees[i][0]-j >= 0) /*if(garden[trees[i][0]-j][trees[i][1]][height] != 0)*/ garden[trees[i][0]-j][trees[i][1]][in_shadow] = 1;
                    if(phase == 2) if(trees[i][1]+j < w)  /*if(garden[trees[i][0]][trees[i][1]+j][height] != 0)*/ garden[trees[i][0]][trees[i][1]+j][in_shadow] = 1;
                }
            }

            FOR(i, trees_amt){  // for trees not in shadow: grow them or cone them
                if(garden[trees[i][0]][trees[i][1]][in_shadow] == 0){
                    if(garden[trees[i][0]][trees[i][1]][height] < 9){
                        garden[trees[i][0]][trees[i][1]][height]++;
                        if(garden[trees[i][0]][trees[i][1]][height] == 9)      // if tree became mature
                            non_grown_trees--;
                    } else cones++;
                } else{
                    if(garden[trees[i][0]][trees[i][1]][not_grown]<3) {
                        garden[trees[i][0]][trees[i][1]][not_grown]++;
                        if(garden[trees[i][0]][trees[i][1]][not_grown] == 3){
                            non_grown_trees--, useful_trees--;
                        }
                    }
                }
            }
            // print_garden(w, h, d, phase);

            // printf("Trees in shadow:\n");
            // FOR(i, h){
            //     FOR(j, w){
            //         printf("%d ", garden[i][j][in_shadow]);
            //     }
            //     printf("\n");
            // }
            // printf("\n");
            // printf("Cones: %lld\n", cones);
        }
        // printf("Day: %lld", d);
        // FOR(i, trees_amt){
        //     printf("%lld ", garden[trees[i][0]][trees[i][1]][not_grown]);
        // }
        // printf("\n");
        // print_garden(h, w, 0, -1);
        
    }

    long long int cones_per_day = 0;
    // printf("Cones calc:\n");
    FOR(i, trees_amt){
        cones_per_day += 3-garden[trees[i][0]][trees[i][1]][not_grown];
        // printf("%lld ", 3-garden[trees[i][0]][trees[i][1]][not_grown]);
    }
    // printf("\n");

    if(d > 0) cones += d*cones_per_day;

    // print_garden(w, h, 0, -1);

    // printf("Phases blockage:\n");
    // FOR(i, trees_amt)
    //     printf("%lld ", garden[trees[i][0]][trees[i][1]][not_grown]);

    // printf("\nEOP, cones: %lld, cones per day: %lld, days left: %lld, non grown trees: %lld, useful trees: %lld\n", cones, cones_per_day, d, non_grown_trees, useful_trees);

    printf("%lld\n", cones);
}
