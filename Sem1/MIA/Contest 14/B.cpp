/* #region */
/* --- LIBRARIES --- */
#include <bits/stdc++.h>
using namespace std;

/* --- SUPERFOR --- */
#define GET_MACRO_FOR(_1, _2, _3, _4, NAME, ...) NAME
#define _FOR3(i, a, n, inc) for (auto i = (a); (inc) > 0 ? i < (n) : i >= (n); i += (inc))
#define _FOR2(i, a, n) _FOR3(i, a, n, 1)
#define _FOR1(i, n) _FOR2(i, 0, n)
#define _FOR0(n) _FOR1(i, n)
#define FOR(...) GET_MACRO_FOR(__VA_ARGS__, _FOR3, _FOR2, _FOR1, _FOR0)(__VA_ARGS__)    //? (/name/, //from//, to, ///inc///)

/* --- VARS --- */
#define ll long long int

/* --- MISC --- */
#define nl putchar('\n');
#define cerr if (debug) cout
#define fastio(on) if(on){ios_base::sync_with_stdio(false); if(on > 1) cin.tie(0); if(on > 2) cout.tie(0);}
/* #endregion */
#define x first
#define y second


ll area1(vector< pair<ll, ll> > tab){
        ll left = max(tab[0].x, tab[2].x);
        ll right = min(tab[1].x, tab[3].x);
        ll down = max(tab[0].y, tab[2].y);
        ll up = min(tab[1].y, tab[3].y);

        if(left > right|| down > up) return 0;
        return (right - left)*(up - down);
}

ll area2(vector< pair<ll, ll> > tab){
        ll left = max(tab[0].x, tab[4].x);
        ll right = min(tab[1].x, tab[5].x);
        ll down = max(tab[0].y, tab[4].y);
        ll up = min(tab[1].y, tab[5].y);

        if(left > right|| down > up) return 0;
        return (right - left)*(up - down);
}

ll area3(vector< pair<ll, ll> > tab){
        ll left = max(max(tab[0].x, tab[2].x), tab[4].x);
        ll right = min(min(tab[1].x, tab[3].x), tab[5].x);
        ll down = max(max(tab[0].y, tab[2].y), tab[4].y);
        ll up = min(min(tab[1].y, tab[3].y), tab[5].y);

        if(left > right|| down > up) return 0;
        return (right - left)*(up - down);
}

    
int main() {
    vector< pair<ll, ll> > tab;

    FOR(6){
        ll x, y; cin >> x >> y;
        tab.push_back({x, y});
    }

    if(area1(tab)+area2(tab)-area3(tab) < abs(tab[1].x - tab[0].x) * abs(tab[1].y - tab[0].y)) cout << "YES\n";
    else cout << "NO\n";
}
