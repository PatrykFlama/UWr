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


int main() {
    fastio(4);
    ll t; cin >> t;

    while(t--){
        vector<ll> tab1;
        vector<ll> tab2;
        
        ll n; cin >> n;
        ll maxx = LONG_MIN;
        ll s = 0;

        for(ll i = 0; i < n; i++){
            ll k;
            cin >> k;
            tab1.push_back(k);
        }

        for(ll i = 0; i < n; i++){
            ll k;
            cin >> k;
            s += k;
            tab2.push_back(k);
        }

        vector< pair<ll, ll> > v3;
        for(ll i = 0; i < n; i++){
            v3.push_back(make_pair(tab1[i], tab2[i]));
        }

        sort(v3.begin(), v3.end());

        ll c = 0;
        ll r = 0;

        for(ll i = n-1; i >= 0; i--){
            c += v3[i].second;

            if(c < v3[i].first) continue;
                
            if(v3[i].first >= c){
                r += v3[i].first;
                break;   
            } else{
                r = max(c - v3[i].second, v3[i].first);
                break;
            }
        }

        if(r != 0)
            cout << r << '\n';
        else
            cout << s << '\n';
    }
}