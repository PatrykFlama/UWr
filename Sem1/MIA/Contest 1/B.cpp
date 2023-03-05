#include<bits/stdc++.h>
using namespace std;
priority_queue< pair<int, int>, vector<pair<int, int> >, std::greater<pair<int, int> > > dist; // int, idx
vector<pair<int, int> > length; // index to, int
#define cerr if(1) cout


int return_index(int index){
    while(length[index].first != index) index = length[index].first;
    return index;
}

int main(){
    int n, m, k; // n of broken, l of stick, max pcs
    cin >> n >> m >> k;
    int temp1, temp2;
    if(n != 0) cin >> temp2;
    for(int i = 0; i < n-1; i++){
        cin >> temp1;
        dist.push({temp1-temp2, i});
        temp2 = temp1;
        length.push_back({i, 1});
    }
    length.push_back({n-1, 1});

    while(dist.size() > k-1){
        int origin = return_index(dist.top().second);
        cerr << "data(idx, l, dst): " << return_index(dist.top().second) << " -> " << origin << ' ' << length[origin].second << ' ' << dist.top().first << '\n';
        length[origin].second += dist.top().first;
        length[dist.top().second+1].first = origin;
        dist.pop();
    }

    for(auto i : length) cerr << i.first << ' ' << i.second << '\n';

    int sum = 0;
    for(int i = 0; i < length.size(); i++){
        if(length[i].first == i) sum += length[i].second;
    }

    cout << sum << '\n';
}