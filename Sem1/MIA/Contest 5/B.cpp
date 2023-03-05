/* #region SUPERFOR */
#define GET_MACRO(_1, _2, _3, _4, NAME, ...) NAME
#define _FOR3(i, a, n, inc) for (auto i = (a); (inc) > 0 ? i < (n) : i >= (n); i += (inc))
#define _FOR2(i, a, n) _FOR3(i, a, n, 1)
#define _FOR1(i, n) _FOR2(i, 0, n)
#define _FOR0(n) _FOR1(i, n)
#define FOR(...) GET_MACRO(__VA_ARGS__, _FOR3, _FOR2, _FOR1, _FOR0)(__VA_ARGS__)    //? (/name/, //from//, to, ///inc///)
#include <bits/stdc++.h>
#define cerr if (debug) cout
using namespace std;
/* #endregion */
const int L = 15e4+5;
vector<int> graph[L];
bool vis[L];
vector<int> to_check;

int dfs(int ptr){
    // cout << "Im in: " << ptr << '\n';
    // FOR(i, 1, 5) cout << vis[i];
    // cout << "\n\n";

    int res = 1;
    to_check.push_back(ptr);
    vis[ptr] = true;
    
    FOR(i, 0, graph[ptr].size()){
        if(!vis[graph[ptr][i]]) res += dfs(graph[ptr][i]);
    }

    return res;
}

bool is_reasonable(int n){
    FOR(i, 1, n+1){
        if(!vis[i]){
            to_check.clear();
            int graph_size = dfs(i)-1;
            // cout << "GS: " << graph_size << '\n';

            for(int j : to_check){
                if(graph[j].size() < graph_size){
                    return false;
                }
            }
        }
    }

    return true;
}

int main(){
    int n, m; cin >> n >> m;
    
    // fill(vis, vis+n+1, 0);

    FOR(m){
        int from, to;
        cin >> from >> to;
        graph[from].push_back(to);
        graph[to].push_back(from);
    }

    cout << (is_reasonable(n) ? "YES\n" : "NO\n");
}