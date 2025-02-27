#include <bits/stdc++.h>
using namespace std;
typedef long long ll;

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    ll a, b;
    cin >> a >> b;

    if(a < b) swap(a, b);

    while(b != 0) {
        ll c = a % b;
        a = b;
        b = c;
    }

    cout << a << '\n';
}
