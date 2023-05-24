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

// const int N = 1e5+5;
// int knowledge[N];
vector<int> knowledge;
vector<int> lost_knowledge;       // index shifted + 1


int main(){
    int n, k; cin >> n >> k;
    long long gained_knowledge = 0;

    FOR(n){
        int amt; cin >> amt;
        knowledge.push_back(amt);
    }

    lost_knowledge.push_back(0);
    FOR(n){
        int mult; cin >> mult;
        gained_knowledge += knowledge[i]*mult;

        lost_knowledge.push_back(lost_knowledge[i]+knowledge[i]*(mult ? 0 : 1));
    }

    int max_more_knowledge = -1;
    FOR(i, k, n+1){
        max_more_knowledge = max(max_more_knowledge, lost_knowledge[i]-lost_knowledge[i-k]);
    }

    // FOR(i, 1, n+1) cout << lost_knowledge[i] << ' ';
    // cout << '\n';

    cout << gained_knowledge + max_more_knowledge << '\n';
}