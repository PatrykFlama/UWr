#define GET_MACRO(_1, _2, _3, _4, NAME, ...) NAME
#define _FOR3(i, a, n, inc) for(auto i = (a); (inc) > 0 ? i < (n) : i >= (n); i += (inc))
#define _FOR2(i, a, n) _FOR3(i, a, n, 1)
#define _FOR1(i, n) _FOR2(i, 0, n)
#define _FOR0(n) _FOR1(i, n)
#define FOR(...) GET_MACRO(__VA_ARGS__, _FOR3, _FOR2, _FOR1, _FOR0)(__VA_ARGS__)

#include <bits/stdc++.h>
using namespace std;
#define debug true
#define cerr if(debug) cout


int scene[15][15];        // [row][column]
int r, c, n, k;


void print_scene(){
    if(debug){
        cerr << "---------\n";
        for(int i = 0; i < r; i++){
            for(int j = 0; j < c; j++) cerr << scene[i][j] << ' ';
            cerr << '\n';
        }
        cerr << "----------\n";
    }
}

int main(){
    cin >> r >> c >> n >> k; //r - rows, c - columns, n - number of violas, k - minimum number of violas to be taken
    for(int i = 0; i < n; i++){
        int x, y; cin >> x >> y;
        scene[--x][--y] = 1;
    }
    /* #region *//*

    print_scene();
    
    for(int i = 1; i < c; i++) // compute 1st row
        scene[0][i] = scene[0][i-1] + scene[0][i];
    for(int i = 1; i < r; i++){     // compute next cell prefixes
        scene[i][0] = scene[i-1][0] + scene[i][0];
        for(int j = 1; j < c; j++){
            scene[i][j] = scene[i][j-1] + scene[i-1][j] - scene[i-1][j-1] + scene[i][j];
        }
    }

    print_scene();
    
    int photos = 0;
    FOR(i, r) FOR(j, c) if(scene[i][j] >= k) photos++;
    FOR(i, r) if(scene[i][0] >= k) photos++;
    FOR(j, r) if(scene[0][j] >= k) photos++;

    FOR(i, r) FOR(j, c){
        FOR(x, i+1, r) FOR(y, j+1, c){
            if(scene[x][y]-scene[i][y]-scene[x][j]+scene[i][j] >= k){
                photos++;
                cerr << "Photo at: " << i << 'x' << j << ' ' << x << 'x' << y << '\n';
            } 
        }
    }

    cout << photos << '\n';
    *//* #endregion */

    int photos = 0;
    FOR(i, r) FOR(j, c){
        FOR(x, i, r) FOR(y, j, c){
            int sum = 0;
            FOR(w, i, x+1) FOR(z, j, y+1) sum += scene[w][z];
            if(sum >= k) photos++;
        }
    }
    cout << photos << '\n';
}