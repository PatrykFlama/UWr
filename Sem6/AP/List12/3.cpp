#include <bits/stdc++.h>
using namespace std;

#define cerr if (1) cout
#define fst first
#define snd second
typedef long long ll;
typedef long double ld;
typedef pair<int, int> pii;



struct Point {
    ll x, y;

    friend istream& operator>>(istream& is, Point& p) {
        return is >> p.x >> p.y;
    }

    Point operator-(const Point& other) const {
        return {x - other.x, y - other.y};
    }
};


ld distanceFromLine(const Point& a, const Point& b, const Point& p) {
    Point ab = b - a;
    Point ap = p - a;

    ll cross = ab.x * ap.y - ab.y * ap.x;

    ld numerator = abs((ld)cross);
    ld denominator = hypotl((ld)ab.x, (ld)ab.y);

    return numerator / denominator;
}

int main() {
    ios::sync_with_stdio(0);
    cin.tie(0);

    int q; cin >> q;

    cout << fixed << setprecision(9);
    
    while (q--) {
        Point a, b, p;
        cin >> a >> b >> p;
        cout << distanceFromLine(a, b, p) << '\n';
    }
}
