#pragma GCC optimize("Ofast","unroll-loops","omit-frame-pointer","inline")  //Optimization flags
#pragma GCC option("march=native","tune=native","no-zero-upper")            //Enable AVX
#pragma GCC target("avx2")                                                  //Enable AVX
#include <x86intrin.h>                                                      //AVX/SSE Extensions


#include <bits/stdc++.h>
using namespace std;

typedef long long ll;

constexpr int L = 13;
int N;
int cols[L], diag_l[2*L], diag_r[2*L];

inline bool check_cell(int r, int c) {
    return cols[c] || diag_l[c+r] || diag_r[c-r+N];
}

inline void place_hetman(int r, int c, int amt) {
    cols[c] += amt;
    diag_l[c+r] += amt;
    diag_r[c-r+N] += amt;
}

ll res = 0;
void solve(int hetmans, int rfrom=0) {
    for(int i = rfrom; i < N; i++) {
        for(int j = 0; j < N; j++) {
            // cell occupied
            if(check_cell(i, j)) continue;

            // place hetman
            cols[j] = 1;
            diag_l[j+i] = 1;
            diag_r[j-i+N] = 1;
            
            // recursive call
            if(hetmans > 1) solve(hetmans-1, i+1);
            else res++;
            
            // remove hetman
            cols[j] = 0;
            diag_l[j+i] = 0;
            diag_r[j-i+N] = 0;
        }
    }
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> N;
    solve(N);
    cout << res << '\n';
}