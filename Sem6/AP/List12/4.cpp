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

    ll cross(const Point& other) const {
        return x * other.y - y * other.x;
    }
};


ll point_cross(const Point& a, const Point& b, const Point& c) {
    return (b - a).cross(c - a);
}

int main() {
    ios::sync_with_stdio(0);
    cin.tie(0);

    ll xq;
    cin >> xq;
    Point q = {xq, 0};
    
    int n;
    cin >> n;
    vector<Point> pts(n);

    for (int i = 0; i < n; ++i) {
        cin >> pts[i];
    }


    // sort by angle with respect to point q (xq, 0)
    sort(pts.begin(), pts.end(), [&](const Point& a, const Point& b) {
        ll cr = point_cross(q, a, b);
        if (cr != 0) return cr > 0;
        
        ll da = (a.x - q.x) * (a.x - q.x) + (a.y - q.y) * (a.y - q.y);
        ll db = (b.x - q.x) * (b.x - q.x) + (b.y - q.y) * (b.y - q.y);
        return da < db;
    });


    int visible = 0;
    Point last; // last visible point
    bool last_exists = false;

    for (const Point& p : pts) {
        if (!last_exists) {
            last = p;
            last_exists = true;
            visible++;
        } else {
            ll cr = point_cross(q, last, p);

            if (cr != 0) {
                last = p;
                visible++;
            }
        }
    }

    cout << visible << '\n';
}
