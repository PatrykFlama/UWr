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

const int L = 60;
int tab[L][L];
vector< pair<int,int> > vec;
int moves[4][2] = {{1,0},{0,1},{-1,0},{0,-1}};
int n;


void dfs(int r, int c){
    if((r < 0 || c < 0) || (tab[r][c] == 1) || (r >= n || c >= n)) return;

    tab[r][c] = 1;
    vec.push_back({r, c});
    FOR(4) dfs(r + moves[i][0], c + moves[i][1]);
}

int main(){
    int r1, r2, c1, c2;
    cin >> n >> r1 >> c1 >> r2 >> c2;
    --r1, --r2, --c1, --c2;

    FOR(i, n){
        string s; cin >> s;
        FOR(j, n) tab[i][j] = s[j] - '0';
    }

    dfs(r1, c1);
    vector< pair<int, int> > from = vec;

    vec.clear();
    dfs(r2, c2);
    vector< pair<int, int> > to = vec;

    int sum = 0, res = LONG_MAX;
    FOR(from.size()){
        FOR(j, to.size()){
            sum =  (from[i].first  - to[j].first ) * (from[i].first  - to[j].first);
            sum += (from[i].second - to[j].second) * (from[i].second - to[j].second);

            res = min(res, sum);
        }
    }

    if(from.size() == 0 || to.size() == 0) res = 0;

    cout << res << '\n';
}
