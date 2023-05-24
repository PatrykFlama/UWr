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


bool func(int n){
    if(n/2 <= 1) return false;
    else for(int i = 2; i*i <= n/2; i++) if(n/2 % i == 0) return false;
    return true;
}

int main(){
    fastio(4);
    ll t; cin >> t;

    while(t--){
        ll n; cin >> n;

        if(n == 1){
            cout << "FastestFinger\n";
            goto end;
        }

        if(n%2 != 0 || n == 2){
            cout << "Ashishgup\n";
            goto end;
        }

        for(ll i = 1; i <= 33; i++){
            if(pow(2, i) == n){
                cout << "FastestFinger\n";
                goto end;
            }
        }     

        if(func(n) && n%2 == 0){
            cout << "FastestFinger\n";
            goto end;
        }

        cout << "Ashishgup\n";
        end:;
    }
}