#include <bits/stdc++.h>
using namespace std;
#define cerr if(0) cout
#define ll long long
#define int ll
#define pii pair<int, int>
#define pll pair<ll, ll>
vector<pii> points; //? x sorted
ll max_scope = 0;


inline static ll perimeter(ll a, ll b, ll c) {
    const ll abx = points[a].first-points[b].first, 
             aby = points[a].second-points[b].second,
             bcx = points[b].first-points[c].first,
             bcy = points[b].second-points[c].second,
             cax = points[c].first-points[a].first,
             cay = points[c].second-points[a].second;

    return abx*abx + aby*aby + bcx*bcx + bcy*bcy + cax*cax + cay*cay;
}


class Triangle {
public:
    int a, b, c;
    ll perimeter;

    Triangle() : Triangle(0, 0, 0) { perimeter = LLONG_MAX; }
    Triangle(int _a, int _b, int _c) : a(_a), b(_b), c(_c) { recalc_perimeter(); }
    
    void recalc_perimeter() {
        perimeter = ::perimeter(a, b, c);
    }

    friend ostream& operator<<(ostream& os, const Triangle& t) {
        os << points[t.a].first << ' ' << points[t.a].second << '\n'
           << points[t.b].first << ' ' << points[t.b].second << '\n'
           << points[t.c].first << ' ' << points[t.c].second << '\n';
        return os;
    }
};


inline Triangle brute_force(int l, int r) {
    Triangle t;
    ll cX = INT_MAX;
    for(int i = l; i <= r; i++) {
        for(int j = i+1; j <= r; j++) {
            for(int k = j+1; k <= r; k++) {
                const ll new_dist = perimeter(i, j, k);
                if(new_dist < cX) {
                    cX = new_dist;
                    t = {i, j, k};
                }
            }
        }
    }

    return t;
}

Triangle find_closest_points(int l, int r) {
    // brute force marginal case
    if(r-l+1 < 6) {
        return brute_force(l, r);
    }

    int mid = (l+r)/2;
    Triangle left = find_closest_points(l, mid);
    Triangle right = find_closest_points(mid+1, r);
    Triangle best = left.perimeter < right.perimeter ? left : right;

    // find points in the middle
    vector<int> middle_right, middle_left;      //? y sorted
    for(int i = mid; i >= l && points[mid].first-points[i].first < best.perimeter; i--) {
        middle_left.push_back(i);
    }
    for(int i = mid+1; i <= r && points[i].first-points[mid].first < best.perimeter; i++) {
        middle_right.push_back(i);
    }

    // sort by y
    sort(middle_left.begin(), middle_left.end(), [&](int a, int b) {
        return points[a].second < points[b].second;
    });
    sort(middle_right.begin(), middle_right.end(), [&](int a, int b) {
        return points[a].second < points[b].second;
    });

    // find best triangle
    // pick one point from left side and 2 from right but they cant be further than best perimeter
    int upper = 0, lower = 0;
    for(int i = 0; i < middle_left.size(); i++) {
        while(upper < middle_right.size() && points[middle_right[upper]].second-points[middle_left[i]].second < best.perimeter) {
            upper++;
        }
        while(lower < upper && points[middle_left[i]].second-points[middle_right[lower]].second > best.perimeter) {
            lower++;
        }

        max_scope = max(max_scope, upper-lower);

        for(int j = lower; j < upper; j++) {
            for(int k = j+1; k < upper; k++) {
                const ll new_dist = perimeter(middle_left[i], middle_right[j], middle_right[k]);
                if(new_dist < best.perimeter) {
                    best = {middle_left[i], middle_right[j], middle_right[k]};
                    best.perimeter = new_dist;
                }
            }
        }
    }

    upper = 0, lower = 0;
    for(int i = 0; i < middle_right.size(); i++) {
        while(upper < middle_left.size() && points[middle_left[upper]].second-points[middle_right[i]].second < best.perimeter) {
            upper++;
        }
        while(lower < upper && points[middle_right[i]].second-points[middle_left[lower]].second > best.perimeter) {
            lower++;
        }

        max_scope = max(max_scope, upper-lower);

        for(int j = lower; j < upper; j++) {
            for(int k = j+1; k < upper; k++) {
                const ll new_dist = perimeter(middle_right[i], middle_left[j], middle_left[k]);
                if(new_dist < best.perimeter) {
                    best = {middle_right[i], middle_left[j], middle_left[k]};
                    best.perimeter = new_dist;
                }
            }
        }
    }

    return best;
}


int32_t main() {
    ios_base::sync_with_stdio(false);
    cin.tie(0);

    int n; cin >> n;
    points.reserve(n);
    for(int i = 0; i < n; i++) {
        int a, b; cin >> a >> b;
        points.push_back({a, b});
    }

    sort(points.begin(), points.end());
    Triangle res = find_closest_points(0, points.size()-1);

    cout << res;
    cout << "Max scope: " << max_scope << '\n';
}
