#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
typedef long long ll;

constexpr int L = 2e5+5;
vector<int> adj[L];
unordered_set<int>* sub_colors[L];
int parent[L];
int color[L];
int outdeg[L];
int res[L];


void solve() {
    queue<int> q;
    for(int i = 1; i < L; i++) {    // O(n)
        if(outdeg[i] == 1) {    // add leaf nodes
            q.push(i);
        }
    }

    while(!q.empty()) {
        int v = q.front();
        q.pop();

        outdeg[parent[v]]--;
        if(parent[v] != v && outdeg[parent[v]] == 1) 
            q.push(parent[v]);

        // calc colors
        // take the max sized set and add to it
        cerr << "finding " << v << ": ";
        int maxx_set = 1, maxx = -1;
        for(int i = 0; i < adj[v].size(); i++) {
            cerr << ' ' << adj[v][i];
            if(adj[v][i] == parent[v]) continue;
            cerr << '*';
            cerr << '(' << maxx << " < " << sub_colors[adj[v][i]]->size() << ", " << (maxx < sub_colors[adj[v][i]]->size()) << ')';
            if(maxx < sub_colors[adj[v][i]]->size()) {
                maxx = sub_colors[adj[v][i]]->size();
                maxx_set = adj[v][i];
                cerr << '&';
            }
        }
        cerr << '\n';
        cerr << "found " << maxx_set << " with size " << maxx << '\n';

        // merge all 
        for(int i = 0; i < adj[v].size(); i++) {
            if(adj[v][i] == parent[v] || adj[v][i] == maxx_set) continue;
            sub_colors[maxx_set]->merge(*sub_colors[adj[v][i]]);
            cerr << "merging " << maxx_set << " <- " << adj[v][i] << ": ";
            for(int i : *sub_colors[maxx_set]) cerr << i << ' ';
            cerr <<'\n';
        }

        // move the result
        if(adj[v].size() > 1) {
            cerr << "moving " << v << " from " << maxx_set << ": ";
            for(int i:*sub_colors[maxx_set]) cerr << i << ' ';
            cerr << '\n';

            delete sub_colors[v];
            sub_colors[v] = sub_colors[maxx_set];
        }
        
        // add this vertex color
        sub_colors[v]->insert(color[v]);

        // save the result
        res[v] = sub_colors[v]->size();
        
        cerr << "analyzed " << v << " new subcols: ";
        for(int i : *sub_colors[v]) cerr << i <<' ';
        cerr << '\n';
    }
}

void find_parents() {
    stack<int> s;
    s.push(1);
    parent[1] = 1;
    outdeg[1]++;

    while(!s.empty()) {
        int v = s.top();
        s.pop();

        for(int u : adj[v]) {
            if(parent[u] != 0) continue;

            parent[u] = v;
            s.push(u);
        }
    }
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    int n; cin >> n;
    for(int i = 1; i <= n; i++) {
        int c; cin >> c;
        color[i] = c;

        sub_colors[i] = new unordered_set<int>();
        sub_colors[i]->insert(c);
    }

    for(int i = 1; i < n; i++) {    // O(n)
        int a, b; cin >> a >> b;

        adj[a].push_back(b);
        adj[b].push_back(a);

        outdeg[a]++;
        outdeg[b]++;
    }

    find_parents();

    solve();

    for(int i = 1; i <= n; i++) {
        cout << res[i] << ' ';
    }
    cout << '\n';
}
