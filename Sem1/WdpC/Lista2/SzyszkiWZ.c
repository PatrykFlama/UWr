#include "stdio.h"
#include "stdbool.h"

const int Rowdir[3] = {0, -1, 0};
const int Coldir[3] = {-1, 0, 1};

const int N = 105;

bool good(int i, int j, int n, int m){
    return (1 <= i && i <= n) && (1 <= j && j <= m);
}

int main(){

    int n, m;
    long long d;

    scanf("%d %d %lld", &m, &n, &d);

    int grid[n + 1][m + 1];

    bool shadow[3][n + 1][m + 1];

    for(int i = 1; i <= n; i++){
        for(int j = 1; j <= m; j++){
            char c; 
            if(scanf(" %c", &c) != 1){
                return 1;
            }
            if(c >= '1' && c <= '9'){
                grid[i][j] = (int) c - '0';
            }else{
                grid[i][j] = 0;
            }
            for(int phase = 0; phase < 3; phase++){
                shadow[phase][i][j] = (grid[i][j] == 0);
            }
        }
    }

    long long ans = 0, cnt = 0, last = d;

    for(int day = 0; day <= d; day++){
        bool growth = false;
        cnt = 0;
        for(int phase = 0; phase < 3; phase++){
            // printf("day = %d, phase = %d:\n", day, phase);
            // for(int i = 1; i <= n; i++){
            //     for(int j = 1; j <= m; j++){
            //         printf("%d ", grid[i][j]);
            //     }
            //     printf("\n");
            // }
            // printf("---\n");
            for(int i = 1; i <= n; i++){
                for(int j = 1; j <= m; j++){
                    int nxt_i = i + Rowdir[phase];
                    int nxt_j = j + Coldir[phase];
                    // int moves = (day == 0 ? grid[i][j] : up[i][j]);
                    int moves = grid[i][j];
                    while(good(nxt_i, nxt_j, n, m) && moves > 0){
                        shadow[phase][nxt_i][nxt_j] = true;
                        nxt_i += Rowdir[phase];
                        nxt_j += Coldir[phase];
                        moves--;
                    }
                }
            }
            if(day == 0)    continue;
            for(int i = 1; i <= n; i++){
                for(int j = 1; j <= m; j++){
                    if(shadow[phase][i][j]){
                        continue;
                    }
                    if(1 <= grid[i][j] && grid[i][j] <= 8){
                        grid[i][j]++;
                        growth = true;
                    }else{
                        cnt++;
                        ans++;
                    }
                }
            }
        }
        if(!growth && day != 0){
            break;
        }
        last--;
    }
    if(last >= 0) ans += last * cnt;
    printf("%lld", ans);
    return 0;
}
