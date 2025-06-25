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

    bool operator<(const Point& other) const {
        return tie(x, y) < tie(other.x, other.y);
    }
};


ll cross(const Point& a, const Point& b, const Point& c) {
    return (b.x - a.x)*(c.y - a.y) - (b.y - a.y)*(c.x - a.x);
}

vector<Point> convexHull(vector<Point>& pts) {
    int n = pts.size();
    if (n <= 1) return pts;

    sort(pts.begin(), pts.end());

    vector<Point> hull;

    // lower part
    for (const auto& p : pts) {
        while (hull.size() >= 2 && cross(hull[hull.size()-2], hull.back(), p) <= 0)
            hull.pop_back();
        hull.push_back(p);
    }

    // upper part
    int lower_size = hull.size();
    for (int i = n - 2; i >= 0; --i) {
        const auto& p = pts[i];
        while (hull.size() > lower_size && cross(hull[hull.size()-2], hull.back(), p) <= 0)
            hull.pop_back();
        hull.push_back(p);
    }

    // last point is a duplicate of first
    hull.pop_back();

    return hull;
}

int main() {
    ios::sync_with_stdio(0);
    cin.tie(0);

    int n; cin >> n;
    vector<Point> pts(n);
    for (int i = 0; i < n; ++i)
        cin >> pts[i].x >> pts[i].y;

    vector<Point> hull = convexHull(pts);
    sort(hull.begin(), hull.end());

    cout << hull.size() << '\n';
    for (const auto& p : hull)
        cout << p.x << ' ' << p.y << '\n';
}
