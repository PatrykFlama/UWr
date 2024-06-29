#include <bits/stdc++.h>
using namespace std;
const int L_PAT = 201;
const int L_TAB = 2001;
string pat[L_PAT];
string tab[L_TAB];
int a, b, c, d;
int r_seq[L_PAT], c_seq[L_PAT];


bool check(int row, int col) {
    int r = 0, c = 0;
    while(r < a) {
        c = 0;
        while(c < b) {
            if(tab[row+r_seq[r]][col+c_seq[c]] != pat[r_seq[r]][c_seq[c]])
                return false;
            ++c;
        }
        ++r;
    }
    return true;
}

void gen_bins_seq(int s, int tab[]) {
    int ptr = 0;
    queue<pair<int, int>> q;
    q.push({0, s});

    while(!q.empty()) {
        auto [l, r] = q.front();
        q.pop();
        int mid = (r+l)/2;
        tab[ptr++] = mid;

        if(l < mid) q.push({l, mid});
        if(mid+1 < r) q.push({mid+1, r});
    }
    tab[ptr++] = s;
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> a >> b >> c >> d;
    for(int i = 0; i < a; i++)  // O(ab) = O(m)
        cin >> pat[i];
    for(int i = 0; i < c; i++) // O(ab) = O(n)
        cin >> tab[i];

    
    // O(sqrt(n))
    gen_bins_seq(a-1, r_seq);
    gen_bins_seq(b-1, c_seq);

    
    int cnt = 0;
    // O(n)
    for(int row = 0; row <= c-a; row++) {
        for(int col = 0; col <= d-b; col++) {
            if(check(row, col)) 
                cnt++;
        }
    }

    cout << cnt << '\n';
}