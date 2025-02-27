#pragma GCC optimize("Ofast","unroll-loops","omit-frame-pointer","inline")  //Optimization flags
#pragma GCC option("march=native","tune=native","no-zero-upper")            //Enable AVX
#pragma GCC target("avx2")                                                  //Enable AVX
#include <x86intrin.h>                                                      //AVX/SSE Extensions


#include <bits/stdc++.h>
using namespace std;

typedef long long ll;

int N;
int res = 0;

// diagonals are current row cells taken by diag attack
void solve(int row=0, int cols=0, int diag_l=0, int diag_r=0) {
    if(row == N) { // each row needs a queen
        res++;
        return;
    }

    int free_cells =((1 << N) - 1) & ~(cols | diag_l | diag_r);
    while(free_cells) {  // for each free cell
        int col = free_cells & -free_cells;
        free_cells ^= col;

        solve(row + 1, cols | col, (diag_l | col) << 1, (diag_r | col) >> 1);
    }
}

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> N;
    solve();
    cout << res << '\n';
}
