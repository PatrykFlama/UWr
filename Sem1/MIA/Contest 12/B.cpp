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
const int L = 300;
const int N = 1e6+5;
int MAX_LETTERS = 0;
int cnt[L], sum = 0;
string letters;



int main(){
    int n; cin >> n >> letters;
    int min_dist = INT_MAX;
    int l = 0;
    
    FOR(i, n){
        if(cnt[letters[i]] == 0){
            MAX_LETTERS++;
            cnt[letters[i]] = 1;
        }
    }

    for(auto &i : cnt) i = 0;

    FOR(r, n){
        
        if(cnt[letters[r]] == 0) sum++;
        cnt[letters[r]]++;

        if(sum == MAX_LETTERS) min_dist = min(min_dist, r-l+1);
        while(sum == MAX_LETTERS){
            cnt[letters[l]]--;
            if(cnt[letters[l]] == 0) sum--;
            else min_dist = min(min_dist, r-l);
            l++;
        }
    }

    cout << min_dist << '\n';
}
