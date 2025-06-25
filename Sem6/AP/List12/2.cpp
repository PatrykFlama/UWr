#include <bits/stdc++.h>
using namespace std;

#define cerr if (1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;


struct Point {
    ll x, y;
};

// Shoelace formula
ll polygonArea(const vector<Point>& poly) {
    ll area2 = 0;
    int n = poly.size();
    for (int i = 0; i < n; ++i) {
        const Point& a = poly[i];
        const Point& b = poly[(i + 1) % n];
        area2 += a.x * b.y - b.x * a.y;
    }
    return abs(area2);
}

int main() {
    ios::sync_with_stdio(0);
    cin.tie(0);

    int n; cin >> n;

    vector<Point> poly(n);
    for (int i = 0; i < n; i++)
        cin >> poly[i].x >> poly[i].y;

    cout << polygonArea(poly) / 2 << (polygonArea(poly) % 2 ? ".5" : ".0") << '\n';
}