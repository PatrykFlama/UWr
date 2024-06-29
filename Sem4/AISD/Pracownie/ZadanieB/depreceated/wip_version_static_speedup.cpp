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
    int ab, bc, ac;
    ll perimeter;

    Triangle() : Triangle(0, 0, 0) { perimeter = LLONG_MAX; }
    Triangle(int _a, int _b, int _c) : a(_a), b(_b), c(_c) { recalc_perimeter(); }
    
    void recalc_perimeter() {
        const ll abx = x[a].first-x[b].first, 
             aby = x[a].second-x[b].second,
             bcx = x[b].first-x[c].first,
             bcy = x[b].second-x[c].second,
             cax = x[c].first-x[a].first,
             cay = x[c].second-x[a].second;

        ab = abx*abx + aby*aby;
        bc = bcx*bcx + bcy*bcy; 
        ac = cax*cax + cay*cay;
        perimeter = ab + bc + ac;
    }

    void update_a(int _a) {
        const ll abx = x[_a].first-x[b].first, 
             aby = x[_a].second-x[b].second,
             cax = x[c].first-x[_a].first,
             cay = x[c].second-x[_a].second;
        perimeter = perimeter - ab - ac + (ab = abx*abx + aby*aby) + (ac = cax*cax + cay*cay);
        a = _a;
    }

    void update_b(int _b) {
        const ll abx = x[a].first-x[_b].first, 
             aby = x[a].second-x[_b].second,
             bcx = x[_b].first-x[c].first,
             bcy = x[_b].second-x[c].second;
        perimeter = perimeter - ab - bc + (ab = abx*abx + aby*aby) + (bc = bcx*bcx + bcy*bcy);
        b = _b;
    }

    void update_c(int _c) {
        const ll bcx = x[b].first-x[_c].first,
             bcy = x[b].second-x[_c].second,
             cax = x[_c].first-x[a].first,
             cay = x[_c].second-x[a].second;
        perimeter = perimeter - bc - ac + (bc = bcx*bcx + bcy*bcy) + (ac = cax*cax + cay*cay);
        c = _c;
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
    Triangle t, tested(0, 1, 2);
    ll perX = INT_MAX;
    for(int i = l; i <= r; i++) {
        tested.update_a(i);
        for(int j = i+1; j <= r; j++) {
            tested.update_b(j);
            for(int k = j+1; k <= r; k++) {
                tested.update_c(k);
                if(tested.perimeter < perX) {
                    perX = tested.perimeter;
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
    const int mid_val = find_x(l, r, x[mid].first);

    // find points in the middle
    vector<int> middle;
    middle.reserve(r-l+1);
    for(int i = mid_val; i >= l && x[mid].first-x[i].first < best.perimeter; i--) {
        middle.push_back(i);
    }
    for(int i = mid_val+1; i <= r && x[i].first-x[mid].first < best.perimeter; i++) {
        middle.push_back(i);
    }

    // sort by y
    sort(middle.begin(), middle.end(), [&](int a, int b) {
        return x[a].second < x[b].second;
    });

    // find best triangle
    for(int i = 0; i < middle.size(); i++) {
        for(int j = i+1; j < middle.size(); j++) {
            for(int k = j+1; k < middle.size(); k++) {
                const ll new_dist = perimeter(middle[i], middle[j], middle[k]);
                if(new_dist < best.perimeter) {
                    best = {middle[i], middle[j], middle[k]};
                    best.recalc_perimeter();
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

    // cerr << "----------------\n";
    cout << res;
    // cerr << res.perimeter << '\n';
}
