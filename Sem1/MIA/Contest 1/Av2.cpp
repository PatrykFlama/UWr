#include <bits/stdc++.h>
using namespace std;
vector<int> even, odd;
#define cerr if(0) cout

int main(){
    int n, len; cin >> len;
    for(int i = 0; i < len; i++){
        cin >> n;
        if(n%2 == 0){
            even.push_back(n);
        } else{
            odd.push_back(n);
        }
    }

    sort(even.begin(), even.end());
    sort(odd.begin(), odd.end());

    int sum = 0;
    if(odd.size() > even.size()){
        int diff = 1;
        //take odd_len - even -1 
        for(int i = 0; i < odd.size()-even.size()-1; i++){
            sum += odd[i];
            cerr << "1new sum: " << sum << ' ';
        }
        // for(int i = 0; i < even.size(); i++){
        //     sum += even[i];
        //     cerr << "2new sum: " << sum << ' ';
        // }
    } else{
        int diff = 1;
        if(even.size() == odd.size()) diff = 0;
        for(int i = 0; i < even.size()-odd.size()-diff; i++){
            sum += even[i];
            cerr << "3new sum: " << sum << ' ';
        }
        // for(int i = 0; i < odd.size(); i++){
        //     sum += odd[i];
        //     cerr << "4new sum: " << sum << ' ';
        // }
    }
    cerr << "\n";

    cout << sum << '\n';
}
