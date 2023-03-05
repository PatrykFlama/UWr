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


vector < pair<int, int> > tab;
bool vis[105];

void dfs(int src){
    pair<int, int> x = tab[src];
    vis[src] = 1;

    FOR(i, tab.size()){
        pair<int, int> y = tab[i];

        if(!vis[i]){
            if((x.first > y.first && x.first < y.second) || (x.second > y.first && x.second < y.second)) dfs(i);
        }
    }
}

int main(){
    fastio(5);

    int n;
    cin >> n;

    while(n--){
        int m, x, y;
        cin >> m >> x >> y;
        
        if(m == 1) tab.push_back(pair<int, int>(x,y));
        else{
            memset(vis, 0, sizeof(vis));
            dfs(x-1);

            if(vis[y-1]) cout << "YES\n";
            else cout << "NO\n";
        }
    }
}
