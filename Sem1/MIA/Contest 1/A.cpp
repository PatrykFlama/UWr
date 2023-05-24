#include <bits/stdc++.h>
using namespace std;
vector<int> odds, evens;

int main(){
    int odd = 0, even = 0, len, n;
    cin >> len;
    for(int i = 0; i < len; i++){
        cin >> n;
        if(n%2 == 0){
            even++;
            evens.push_back(n);
        } else{
            odd++;
            odds.push_back(n);
        }
    }

    sort(evens.begin(), evens.end());
    sort(odds.begin(), odds.end());

    // cout << even << '\n';
    // for(int i : evens) cout << i << ' ';
    // cout << '\n' << odd << '\n';
    // for(int i : odds) cout << i << ' ';
    // cout << '\n';

    int sum = 0;
    if(odd>even){
        int diff = (even>0 ? 2 : 1);
        for(int i = 0; i < evens.size()-even; i++){
            sum += evens[i];
        } 
        for(int i = 0; i < max(int(odds.size()-even-diff), 0); i++){
            sum += odds[i];
        }
    } else{
        int diff = (odd>0 ? 2 : 1);
        for(int i = 0; i < max(int(evens.size()-odd-diff), 0); i++){
            sum += evens[i];
        } 
        for(int i = 0; i < odds.size()-odd; i++){
            sum += odds[i];
        }
    }
    cout << sum << '\n';
}