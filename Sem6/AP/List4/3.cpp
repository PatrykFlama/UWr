#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
typedef long long ll;

constexpr int L = 1e6 + 5;
constexpr int strip_size = 1e3;

/*
we can notice that C*L*sqrt(L) <= Ce9

lets split entire plane into sqrt(L) strips (feg horizontal)

lets focus on one strip, and all points in it - if we order them by ox, then it would be optimal x-wise ordering (giving max dist L)
this for all strips we would need at max sqrt(L) * L dist for ox

now for the oy: the cumulative distance in the strips is at most L/sqrt(L) for each point, thus L*L/srqt(L) = L*sqrt(L) in total
additionally there is cost of traversing between strips, which is at most L/sqrt(L) * sqrt(L) = L

this gives us the total of C*L*sqrt(L) <= Ce9
*/

struct Point {
    int idx, x, y;
};

Point points[L];

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n; cin >> n;
    for (int i = 0; i < n; i++) {
        points[i].idx = i+1;
        cin >> points[i].x >> points[i].y;
    }

    sort(points, points + n, [](const Point &a, const Point &b) {
        // if points in same strip then sort by x (asc or desc depending on strip pairity), else sort by strip number
        if(a.y/strip_size == b.y/strip_size) {
            return (a.y/strip_size & 1) ? a.x > b.x : a.x < b.x;
        } else {
            return a.y < b.y;
        }
    });

    for(int i = 0; i < n; i++) {
        cout << points[i].idx << ' ';
    }
    cout << '\n';
}