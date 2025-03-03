#include <bits/stdc++.h>
using namespace std;

typedef long long ll;

const int L = 1e6+2;
int res[L];

void preprocess() {
    for(int i = 1; i < L; i++) {
        for(int j = i; j < L; j += i) {
            res[j]++;
        }
    }
}

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    preprocess();

    int t; cin >> t;
    while(t--) {
        int a; cin >> a;
        cout << res[a] << '\n';
    }
}