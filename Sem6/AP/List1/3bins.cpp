#include <bits/stdc++.h>
using namespace std;

typedef long long ll;

constexpr int L = 1e5+2;
int N;
int tab[L];

int find(int val) {
    int l = 0, r = N;
    while(l < r) {
        int mid = (l+r)/2;
        if(tab[mid] < val) {
            l = mid+1;
        } else {
            r = mid;
        }
    }

    return (tab[l] == val ? l+1 : -1);
}

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> N;

    for(int i = 0; i < N; i++) {
        cin >> tab[i]; 
    }

    int m; cin >> m;
    while(m--) {
        int q; cin >> q;
        cout << find(q) << '\n';
    }
}