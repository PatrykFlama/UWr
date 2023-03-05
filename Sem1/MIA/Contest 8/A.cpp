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
    long long n, m; cin >> n >> m;

    cout << (((n/m))*((n/m)-1)/2) * (m-(n%m)) + (((n/m)+1)*((n/m))/2) * (n%m) << ' ';
    cout << (n-m+1)*(n-m)/2 << '\n'; //max - put one person in each group and all last people to random one
}