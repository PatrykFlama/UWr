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

const int N = 1000+5;
vector<int> graph[N];
bool vis[N];
int n;


int bfs(int v){
    fill(vis, vis+n+1, false);
    queue<int> q;
    q.push(v);
    vis[v] = 1;

    while(!q.empty()){
        int s = q.front();
        q.pop();

        FOR(graph[s].size()){
            v = graph[s][i];

            if(!vis[v]){
                vis[v] = 1;
                q.push(v);
            }
        }
    }

    return v;
}


int main(){
    cin >> n;
    
    FOR(i, 1, n+1) {
        int j; cin >> j;
        graph[i].push_back(j);
    }

    FOR(i, 1, n+1){
        cout << bfs(i) << ' ';
    }

    cout << '\n';
}
