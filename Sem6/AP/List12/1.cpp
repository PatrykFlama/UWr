#include <bits/stdc++.h>
using namespace std;

#define cerr if (1) cout
#define fst first
#define snd second
typedef long long ll;
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


int main() {
    ios::sync_with_stdio(0);
    cin.tie(0);

    int T;
    cin >> T;
    while (T--) {
        Point p1, p2, p3;
        cin >> p1 >> p2 >> p3;

        const Point v1 = p2 - p1;
        const Point v2 = p3 - p1;

        ll cross = v1.x * v2.y - v1.y * v2.x;

        
        if (cross == 0)
            cout << "TOUCH\n";
        else if (cross > 0)
            cout << "LEFT\n";
        else
            cout << "RIGHT\n";
    }
}
