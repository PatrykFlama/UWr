#include <bits/stdc++.h>
using namespace std;
#define ll long long
#define pll pair<ll, ll>
vector<pll> tab;


inline ll segment(ll a, ll b) {
    const ll vf = abs(tab[a].first-tab[b].first), vs = abs(tab[a].second-tab[b].second);
    return sqrt(vf*vf + vs*vs);
}

inline ll circuit(ll a, ll b, ll c) {
    return segment(a, b) + segment(b, c) + segment(c, a);
}

class Triangle {
public:
    ll a, b, c;

    Triangle() : Triangle(LONG_LONG_MAX, LONG_LONG_MAX, LONG_LONG_MAX) {}
    Triangle(ll _a, ll _b, ll _c) : a(_a), b(_b), c(_c) {}
    
    inline ll circuit() {
        return segment(a, b) + segment(b, c) + segment(c, a);
    }

    friend ostream& operator<<(ostream& os, const Triangle& t) {
        os << "" << tab[t.a].first << " " << tab[t.a].second << "\n"
           << "" << tab[t.b].first << " " << tab[t.b].second << "\n"
           << "" << tab[t.c].first << " " << tab[t.c].second << "\n";
        return os;
    }
};

Triangle brute(ll l, ll r){
    Triangle t;
    ll cX = LONG_LONG_MAX;
    for(ll i = l; i <= r; i++){
        for(ll j = i+1; j <= r; j++) {
            for(ll k = j+1; k <= r; k++) {
                const ll new_dist = circuit(i, j, k);
                if(new_dist < cX) {
                    cX = new_dist;
                    t = {i, j, k};
                }
            }
        }
    }

    return t;
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(0);

    ll n; cin >> n;
    tab.reserve(n);
    for(ll i = 0; i < n; i++) {
        ll a, b; cin >> a >> b;
        tab.push_back({a, b});
    }

    Triangle res = brute(0, tab.size()-1);
    cout << res << res.circuit() << "\n";
}