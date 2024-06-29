#include <bits/stdc++.h>
using namespace std;
const int L_PAT = 201;
const int L_TAB = 2001;
string pat[L_PAT];
string tab[L_TAB];
int a, b, c, d;


bool check(int row, int col) {
    int r = 0, c = 0;
    while(r < a) {
        c = 0;
        while(c < b) {
            if(tab[row+r][col+c] != pat[r][c])
                return false;
            ++c;
        }
        ++r;
    }
    return true;
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> a >> b >> c >> d;

    for(int i = 0; i < a; i++)
        cin >> pat[i];
    for(int i = 0; i < c; i++)
        cin >> tab[i];

    
    int cnt = 0;
    for(int row = 0; row <= c-a; row++) {
        for(int col = 0; col <= d-b; col++) {
            if(check(row, col)) 
                cnt++;
        }
    }

    cout << cnt << '\n';
}