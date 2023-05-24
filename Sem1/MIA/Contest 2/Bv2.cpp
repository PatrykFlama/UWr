#include <bits/stdc++.h>
using namespace std;
#define cerr if(1) cout

struct S{
    int links, sum, id;

    S(int n1, int n2, int n3) : links(n1), sum(n2), id(n3){}

    bool operator<(const struct S& s2) const{
        //Your priority logic goes here
        if(links != s2.links) return links > s2.links;
        return sum > s2.sum;
    }
};

vector< vector<int> > tree; // {{v, no_links, link1, ...}, ...}
priority_queue<S> pq;


int main(){
    int n, m; cin >> n >> m;
    tree.resize(n+1);
    for(int i = 1; i <= n; i++){
        int v; cin >> v;
        tree[i].push_back(v);
        tree[i].push_back(0);
    }
    for(int i = 0; i < m; i++){
        int x, y; cin >> x >> y;
        tree[x].push_back(y);
        tree[y].push_back(x);
        tree[x][1]++;
        tree[y][1]++;
    }

    for(int i = 1; i < tree.size(); i++){
        int sum = 0;
        for(int j = 2; j < tree[i].size(); j++){
            sum += tree[tree[i][j]][0];
        }
        pq.push({int(tree[i].size())-2, sum, i});
    }
    // while(!pq.empty()){
    //     cout << pq.top().id << ' ' << pq.top().links << ' ' << pq.top().sum << '\n';
    //     pq.pop();
    // }
    int price = 0;
    while(!pq.empty()){
        auto s = pq.top();
        pq.pop();
        cerr << s.id << ' ' << s.links << ' ' << s.sum << ' ';
        
        if(tree[s.id][0] != -1){
            tree[s.id][0] = -1;
            if(tree[s.id][1] == 0) continue;
            price += s.sum;
            
            for(int i = 2; i < tree[s.id].size(); i++){
                tree[tree[s.id][i]][1]--;
            }

            for(int i = 2; i < tree[s.id].size(); i++){
                int sum = 0;
                for(int j = 2; j < tree[i].size(); j++){
                    sum += max(tree[tree[i][j]][0], 0);
                }
                pq.push({int(tree[i].size())-2, sum, i});
            }
            cerr << "added";
        }
        cerr << '\n';
    }

    cout << price << '\n';
}