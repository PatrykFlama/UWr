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

/* --- MISC --- */
#define cerr if (debug) cout
/* #endregion */


int main(){
    int n, x;
    cin >> n >> x;

    int cnt = 0;
    for(int i = 1; i <= n; i++){
        if(x % i == 0 && x/i <= n){
            // cout << x/i << ' ' << i << '\n';
            cnt++;
            // if(x/i != i) {
            //     cout << i << ' ' << x/i << '\n';
            //     cnt++;
            // }
        }
    }
    cout << cnt << '\n';
}