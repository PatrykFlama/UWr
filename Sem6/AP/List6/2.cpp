#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;


constexpr int L = 2e5+5;
vector<int> graph[L];
int dp[L][2];   // 0 -> take not, 1 -> take do


void dfs(int v, int p) {
    // calc dp[v][0] first
    for(int u : graph[v]) {
        if(u == p) continue;
        dfs(u, v);
        dp[v][0] += max(dp[u][0], dp[u][1]);
    }

    // calc dp[v][1]
    for(int u : graph[v]) {
        if(u == p) continue;
        // new edge + u not matched + v not matched but without calced in child res
        dp[v][1] = max(dp[v][1], 1 + dp[u][0] + (dp[v][0] - max(dp[u][0], dp[u][1])));
    }
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n; cin >> n;

    for(int i = 1; i < n; i++) {
        int a, b; cin >> a >> b;
        graph[a].push_back(b);
        graph[b].push_back(a);
    }

    dfs(1, 0);

    cout << max(dp[1][0], dp[1][1]) << '\n';
}
