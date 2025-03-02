#include <bits/stdc++.h>
using namespace std;

typedef long long ll;
typedef pair<int, int> pii;

int d;
pii ext_eucl_rec(int a, int b) {
    if(b == 0) {
        d = a;
        return {1, 0};
    }

    pii p = ext_eucl_rec(b, a % b);
    return {p.second, p.first - a / b * p.second};
}



int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int t; cin >> t;
    while(t--) {
        int a, b; cin >> a >> b;
        pii p = ext_eucl_rec(a, b);
        cout << p.first << ' ' << p.second << ' ' << d << '\n';
    }
}