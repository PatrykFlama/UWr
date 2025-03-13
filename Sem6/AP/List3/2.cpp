#include <bits/stdc++.h>
using namespace std;

const int L = 2e5+5;
vector<int> tree[L];
int colors[L];
int result[L];
vector<unordered_set<int>*> color_sets;

void dfs(int v, int p) {
    unordered_set<int>* maxx_set = 0;

    for(int u : tree[v]) {
        if(u == p) continue;
        dfs(u, v);

        if(!maxx_set || color_sets[u]->size() > maxx_set->size()) {
            maxx_set = color_sets[u];
        }
    }

    if(!maxx_set) maxx_set = new unordered_set<int>();
    maxx_set->insert(colors[v]);

    for(int u : tree[v]) {
        if(u == p || color_sets[u] == maxx_set) continue;
        // maxx_set->insert(color_sets[u]->begin(), color_sets[u]->end());
        maxx_set->merge(*color_sets[u]);
        delete color_sets[u];
    }

    color_sets[v] = maxx_set;
    result[v] = maxx_set->size();
}

int main() {
    ios::sync_with_stdio(0);
    cin.tie(0);

    int n;
    cin >> n;
 
    for(int i = 1; i <= n; i++) {
        cin >> colors[i];
    }

    for(int i = 0; i < n - 1; i++) {
        int a, b;
        cin >> a >> b;

        tree[a].push_back(b);
        tree[b].push_back(a);
    }

    color_sets.resize(n + 1, 0);
    dfs(1, -1);

    for(int i = 1; i <= n; i++) {
        cout << result[i] << ' ';
    }
    cout << '\n';
}
