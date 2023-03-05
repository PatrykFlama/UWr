// #region
/* --- LIBRARIES --- */
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

/* --- SUPERFOR --- */
#define GET_MACRO_FOR(_1, _2, _3, _4, NAME, ...) NAME
#define _FOR3(i, a, n, inc) for (int i = (a); (inc) > 0 ? i < (n) : i >= (n); i += (inc))
#define _FOR2(i, a, n) _FOR3(i, a, n, 1)
#define _FOR1(i, n) _FOR2(i, 0, n)
#define _FOR0(n) _FOR1(i, n)
#define FOR(...) GET_MACRO_FOR(__VA_ARGS__, _FOR3, _FOR2, _FOR1, _FOR0)(__VA_ARGS__)    //? (/name/, //from//, to, ///inc///)

/* --- MISC --- */
#define fill(tab, len, what) FOR(fill_template_iterator, len) tab[fill_template_iterator] = what;

#define cerr if (debug) cout
// #endregion
// const int N = 1024;
int directions[4][2] = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};
int picture[1050][1050];     // 0->yellow, 1->blue / row,col
int n;


bool safe(int x, int y){
    return (x >= 0 && y >= 0 && x < n && y < n);
}

int max(int a, int b){
    if(a > b) return a;
    return b;
}

void dfs(int row, int col){
    picture[row][col] = -1;

    FOR(i, 4){
        if(safe(row+directions[i][0], col+directions[i][1])){
            if(picture[row+directions[i][0]][col+directions[i][1]] == 1){
                dfs(row+directions[i][0], col+directions[i][1]);
            }
        }
    }
}


int main(){
    scanf("%d", &n);

    FOR(i, n) FOR(j, n){
        int temp;
        scanf("%d", &temp);
        picture[i][j] = temp;
    }

    // -------- SECTION 1 ---------
    // c array problems abomination
    int row_max, snake_max, diagonal_max;
    int maxx = 0;
    int counter = 0;

    // ----row----
    FOR(row, n){
        FOR(col, n){
            if(picture[row][col] == 1) counter++;
            else{
                maxx = max(maxx, counter);
                counter = 0;
            }
        }
    }

    row_max = maxx;

    // ----snake----
    int dir = 1;

    maxx = 0;
    counter = 0;

    FOR(row, n){
        for(int col = (dir == 1 ? 0 : n-1); col >= 0 && col < n; col += dir){
            if(picture[row][col] == 1) counter++;
            else{
                maxx = max(maxx, counter);
                counter = 0;
            }
        }
        dir *= -1;
    }

    snake_max = maxx;

    // ----diagonal-----
    bool border_moved = false;
    int dir_row = 0, dir_col = 0;
    int snake_row = 0, snake_col = 0;

    maxx = 0;
    counter = 0;
    
    while(snake_row < (n-1) || snake_col < (n-1)){
        if(picture[snake_row][snake_col] == 1) counter++;
        else{
            maxx = max(maxx, counter);
            counter = 0;
        }

        if(snake_row == n-1 || snake_col == n-1 || snake_row == 0 || snake_col == 0){
            if(!border_moved){
                border_moved = true;
                if(snake_col == n-1) dir_row = 1, dir_col = 0;            // go down 
                else if(snake_row == n-1) dir_row = 0, dir_col = 1;       // go right
                else if(snake_row == 0) dir_row = 0, dir_col = 1;         // go right
                else dir_row = 1, dir_col = 0;                      // go down
            } else{                                             // restore
                border_moved = false;

                if((snake_row+snake_col) % 2 == 0){
                    dir_row = -1, dir_col = 1;
                } else{
                    dir_row = 1, dir_col = -1;
                }
            } 
        }

        snake_row += dir_row, snake_col += dir_col;
    }

    if(picture[snake_row][snake_row] == 1) counter++;
    else{
        maxx = max(maxx, counter);
        counter = 0;
    }

    diagonal_max = maxx;

    // ------ output1 --------
    printf("%d %d %d\n", row_max, snake_max, diagonal_max);

    // -------- SECTION 2 ---------
    if(n%3 != 0) return 0;
    counter = 0;
    // int queue[n*n][2]; int ptr = 0;

    FOR(row, n){
        FOR(col, n){
            if(picture[row][col] == 1){
                counter++;
                dfs(row, col);
                // printf("row/col %d %d\n", row, col);
                // FOR(r, n){
                //     FOR(c, n){
                //         printf("%d ", picture[r][c]);
                //     }
                //     printf("\n");
                // }
                // // --- bfs ---
                // ptr = 0;
                // queue[ptr][0] = row, queue[ptr][1] = col;
                // while(ptr >= 0){
                //     int bfs_row = queue[ptr][0]; int bfs_col = queue[ptr--][1];
                //     picture[bfs_row][bfs_col] = -1;
                //     FOR(i, 4){
                //         if(safe(bfs_row+directions[i][0], bfs_col+directions[i][1])){
                //             if(picture[bfs_row+directions[i][0]][bfs_col+directions[i][1]] == 1){
                //                 queue[++ptr][0] = bfs_row+directions[i][0]; queue[ptr][1] = bfs_col+directions[i][1];
                //             }
                //         }
                //     }
                // }
            }
        }
    }

    printf("%d\n", counter);
}