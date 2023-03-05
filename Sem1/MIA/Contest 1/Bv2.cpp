#include<bits/stdc++.h>
using namespace std;
#define cerr if(0) cout
priority_queue<int, vector<int>, std::greater<int> > diff;


int main(){
    int n, m, k; // n of broken, l of stick, max pcs
    cin >> n >> m >> k;
    int f, s;
    if(n != 0) cin >> f;
    for(int i = 0; i < n-1; i++){
        cin >> s;
        diff.push(s-f);
        f = s;
    }

    int max_sum = 0;
    while(diff.size() >= k){        // reduce to k-1 amnt of separations => k-1+1 of segments
        // cerr << diff.top() << ' ';
        max_sum += diff.top();
        diff.pop();
    }
    // cerr << " | ";
    // while(!diff.empty()){
    //     cerr << diff.top() << ' ';
    //     diff.pop();
    // }
    // cerr << '\n';

    cout << max_sum + k << '\n';        // increase tape length by k - amount of standalone pieces
}