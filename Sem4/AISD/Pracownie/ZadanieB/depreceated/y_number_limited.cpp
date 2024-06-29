#include <bits/stdc++.h>
using namespace std;
#define cerr if(1) cout
#define ll long long
#define int ll
#define pii pair<int, int>
#define pll pair<ll, ll>
vector<pii> x;


inline static ll perimeter(ll a, ll b, ll c) {
    const ll abx = x[a].first-x[b].first, 
             aby = x[a].second-x[b].second,
             bcx = x[b].first-x[c].first,
             bcy = x[b].second-x[c].second,
             cax = x[c].first-x[a].first,
             cay = x[c].second-x[a].second;

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
        os << x[t.a].first << ' ' << x[t.a].second << '\n'
           << x[t.b].first << ' ' << x[t.b].second << '\n'
           << x[t.c].first << ' ' << x[t.c].second << '\n';
        return os;
    }
};


int find_x(int l, int r, int val) {
    while(l < r) {
        int mid = (l+r)/2;
        if(x[mid].first < val)
            l = mid+1;
        else r = mid;
    }

    return l;
}


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
    vector<int> middle_right, middle_left;
    for(int i = mid; i >= l && x[mid].first-x[i].first < best.perimeter; i--) {
        middle_left.push_back(i);
    }
    for(int i = mid+1; i <= r && x[i].first-x[mid].first < best.perimeter; i++) {
        middle_right.push_back(i);
    }

    // sort by y
    sort(middle_left.begin(), middle_left.end(), [&](int a, int b) {
        return x[a].second < x[b].second;
    });
    sort(middle_right.begin(), middle_right.end(), [&](int a, int b) {
        return x[a].second < x[b].second;
    });

    // find best triangle, 
    // for each point on one side, there can be at most 17 points on the other side
    // left side
    for(int i = 0; i < middle_left.size(); i++) {
        for(int j = 0; j < middle_right.size(); j++) {
            if(x[middle_right[j]].second-x[middle_left[i]].second >= best.perimeter)
                break;

            for(int k = 1; k <= 17 && j+k < middle_right.size(); k++) {
                const ll new_dist = perimeter(middle_left[i], middle_right[j], middle_right[j+k]);
                if(new_dist < best.perimeter) {
                    best = {middle_left[i], middle_right[j], middle_right[j+k]};
                }
            }
        }
    }

    // right side
    for(int i = 0; i < middle_right.size(); i++) {
        for(int j = 0; j < middle_left.size(); j++) {
            if(x[middle_left[j]].second-x[middle_right[i]].second >= best.perimeter)
                break;

            for(int k = 1; k <= 17 && j+k < middle_left.size(); k++) {
                const ll new_dist = perimeter(middle_right[i], middle_left[j], middle_left[j+k]);
                if(new_dist < best.perimeter) {
                    best = {middle_right[i], middle_left[j], middle_left[j+k]};
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
    x.reserve(n);
    for(int i = 0; i < n; i++) {
        int a, b; cin >> a >> b;
        x.push_back({a, b});
    }

    sort(x.begin(), x.end());
    Triangle res = find_closest_points(0, x.size()-1);

    cout << res;
    // cerr << res.perimeter << '\n';
}
