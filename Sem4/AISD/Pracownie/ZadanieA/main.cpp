#include <bits/stdc++.h>
using namespace std;
#define pii pair<int, int>

const int L = 1e6 + 5;
pii tree[L];
int pre[L];
int post[L];
int parent_positions[L];
int t = 0, n;

// int find_vertex(int v) {
//     int l = 0, r = n-2;

//     while(l < r) {
//         int mid = (l+r)/2;
//         if(tree[mid].first < v) {
//             l = mid + 1;
//         } else {
//             r = mid;
//         }
//     }

//     return r;
// }

void dfs(int v) {
    // stacks have {vertex, depth}
    stack<pii> instack;     // vertices to enter to with dfs
    stack<pii> outstack;    // vertices waiting to go out from

    instack.push({v, 0});
    outstack.push({v, 0});

    while(!instack.empty()) {
        const pii ins = instack.top();
        instack.pop();
        const int u = ins.first;
        const int depth = ins.second;

        while(!outstack.empty() && outstack.top().second >= depth) {
            post[outstack.top().first] = t++;
            outstack.pop();
        }

        pre[u] = t++;
        outstack.push({u, depth});

        // int i = find_vertex(u);
        int i = parent_positions[u];
        while(tree[i].first == u) {
            instack.push({tree[i].second, depth+1});    //? graf skierowany, krawędzie tylko w dół
            ++i;
        }
    }

    while(!outstack.empty()) {
        post[outstack.top().first] = t++;
        outstack.pop();
    }
}

int main(){
    ios_base::sync_with_stdio(false);
    cin.tie(0);

    int q;
    cin >> n >> q;

    for (int i = 0; i < n-1; i++) {
        int p; cin >> p;
        tree[i] = {p, i+2};
    }

    sort(tree, tree + n - 1);

    int last = -1;
    for(int i = 0; i < n; i++) {
        if(tree[i].first != last) {
            parent_positions[tree[i].first] = i;
            last = tree[i].first;
        }
    }

    dfs(1);

    while(q--){
        int from, thru; cin >> thru >> from;
        if(pre[from] > pre[thru] && post[from] < post[thru])
            cout << "TAK\n";
        else cout << "NIE\n";
    }
}
