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

int cnt[30];

int main(){
    string s; cin >> s;

    FOR(s.size()) cnt[s[i]-'a']++;

    // FOR(4) cout << cnt[i] << '\n';

    int diff = 0;
    int ptr = 0;
    while(cnt[ptr]%2 == 0 && ptr <= ('z'-'a')) ptr++;
    ptr++;

    while(ptr <= ('z'-'a')){
        if(cnt[ptr]%2)diff++;
        ptr++;
    }

    // cout << diff << '\n';

    if(diff%2) cout << "Second\n";
    else cout << "First\n";
}

