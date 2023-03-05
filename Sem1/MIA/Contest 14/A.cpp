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



int main(){
    // fastio(3);

    ll n, m, x_max, y_max, x_min, y_min;
    ll x[4], y[4], ans;
    y_max = x_max = LONG_MIN;
    y_min = x_min = LONG_MAX;
    cin >> n;

    FOR(n){
        cin >> x[i] >> y[i];
        if(x[i] > x_max)
            x_max = x[i];
        if(x[i] < x_min)
            x_min = x[i];
        if(y[i] > y_max)
            y_max = y[i];
        if(y[i] < y_min)
            y_min = y[i];
    }
    if(n == 1)
        ans = -1;
    else if(n == 2){
        if((x[0] == x[1]) || (y[1] == y[0]))
            ans = -1;
        else
            ans = abs(x[0]-x[1]) * abs(y[1]-y[0]);
    }
    else
        ans = abs(x_max-x_min) * abs(y_max-y_min);
    if(ans == 0)
        cout << "-1\n";
    else
        cout << ans << '\n';
}
