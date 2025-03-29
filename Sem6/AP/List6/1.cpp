#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;


// lemma: path always will start and end with leaf (proof by contradiction)
// lemma: furthest point from any point is edge of path

constexpr int L = 2e5+5;
vector<int> graph[L];
int n;


pii find_furthest(int start) {  //? returns {v, dist}
    queue<pii> q;
    vector<bool> vis(n+5, false);
    int max_dist = 0, max_v = start;

    q.push({start, 0});
    vis[start] = true;

    while(!q.empty()) {
        auto [v, dist] = q.front(); q.pop();

        if(dist > max_dist) {
            max_dist = dist;
            max_v = v;
        }

        for(int u : graph[v]) {
            if(!vis[u]) {
                vis[u] = true;
                q.push({u, dist + 1});
            }
        }
    }

    return {max_v, max_dist};
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> n;

    for(int i = 1; i < n; i++) {
        int a, b; cin >> a >> b;
        graph[a].push_back(b);
        graph[b].push_back(a);
    }

    auto [v1, d1] = find_furthest(1);
    auto [v2, d2] = find_furthest(v1);

    cout << d2 << '\n';
}