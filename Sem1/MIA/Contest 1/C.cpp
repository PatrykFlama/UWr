#include<bits/stdc++.h>
using namespace std;
#define cerr if(1) cout
#define ll long long
ll tab[505][505];      // wiersz - kolumna

// void print_out(int n, int m, int h = -1, int w = -1, bool hp = 0){
//     if(hp == 0) return;
//     cerr << "______\n";

//     for(int i = 0; i < n; i++) {
//         for(int j = 0; j < m; j++) {
//             cerr << tab[i][j] << ' ';
//         }
//         cerr << '\n';
//     }
    
//     if(h != -1) cerr << h << ' ' << w << '\n';
//     cerr << "______\n";
// }

int main(){
    ll tc, num, t = 1;
    ll row, col; cin >> row >> col;

    vector <ll> first[row + col],last[row + col];

    for(ll i = 0; i < row; i++){
       for(ll j = 0; j < col; j++){
           ll x; cin >> x;
           first[i + j].push_back(x);
       }
    }

    for(ll i = 0; i < row; i++){
        for(ll j = 0; j < col; j++){
            ll x; cin >> x;
            last[i + j].push_back(x);
        }
    }


    for(ll i = 0; i < row + col; i++) {
        sort(first[i].begin(), first[i].end());
        sort(last[i].begin(), last[i].end());
    }

    for(ll i = 0; i < row + col; i++) {
        if(first[i] != last[i]) return cout << "NO" << endl,0;
    }
    cout << "YES" << endl;

    return 0;
}

/*
5 4
6 3 2 11 5 9 4 2 3 3 3 3 4 8 2 2 7 8 6 4
6 3 2 11 5 9 3 8 3 4 3 2 4 2 3 2 7 8 6 4

5 4 6 3 2 11 5 9 4 2 3 3 3 3 4 8 2 2 7 8 6 4 6 3 2 11 5 9 3 8 3 4 3 2 4 2 3 2 7 8 6 4
*/