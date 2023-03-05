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
#define ll long long

/* --- MISC --- */
#define nl putchar('\n');
#define cerr if (debug) cout
#define fastio(on) if(on){ios_base::sync_with_stdio(false); if(on > 1) cin.tie(0); if(on > 2) cout.tie(0);}
/* #endregion */
const double PI = 3.14159265358979323;
ll dp[1000000];
#define V first
#define idx second


bool cmp(pair<ll, int> a, pair<ll, int> b){
    if (a.first != b.first) return a.first < b.first;
    return a.second>b.second;
}

ll query(ll n, ll l, ll r, ll v1, ll v2){  
    if(l > r || v1 > r || v2 < l) return 0;
    if(v1 <= l && v2 >= r) return dp[n];
    return max(query(2*n, l, (l+r)/2, v1, v2), query(2*n+1, ((l+r)/2)+1, r, v1, v2));
}

void update(ll n, ll l, ll r, ll v1, ll v2, ll val){
    if(v1 > r || v2 < l || l > r) return;

    if(v1 <= l && v2 >= r){
        dp[n] += val;
        return;
    }

    update(2*n, l, (l+r)/2, v1, v2, val);
    update(2*n+1, ((l+r)/2)+1, r, v1, v2, val);
    
    dp[n] = max(dp[2*n], dp[2*n+1]);
}

int main(){
    fastio(4);
    ll n; cin >> n;
    vector< pair<ll, ll> > tab;

    FOR(i, n){
        ll v, h;
        cin >> v >> h;
        tab.push_back({v*v*h, i});
    }
    
    sort(tab.begin(), tab.end(), cmp);

    FOR(i, n){
        ll maxx = query(1, 0, n-1, 0, tab[i].idx-1);
        update(1, 0, n-1, tab[i].idx, tab[i].idx, maxx+tab[i].V);
    }

    printf("%.9lf\n", PI * (double)(query(1, 0, n-1, 0, n-1)));
}
