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
#define ll long long
const int L = 1e5+5;
ll tab[L];


int main(){
    ll n, diff_max; cin >> n >> diff_max;
    ll diff = 0, points = 0;

    cin >> tab[0];
    FOR(i, 1, n) {
        cin >> tab[i];
    }

    ll l = 0, r = 2;
    ll sum = 0;
    while(r < n){
        while(r < n && abs(tab[r]-tab[l]) <= diff_max) r++;
        r--;

        // cout << "l/r: " << l << '/' << r << '\n';

        sum += (r-l)*(r-l-1)/2;

        l++;
        if(r <= l+1) r = l+2;
    }

    cout << sum << '\n';
}
