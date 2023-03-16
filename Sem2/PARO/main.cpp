// https://codeforces.com/group/dnrswkaLnn/contest/423841/problem/A
/*
Input:
2
0 0
1 1
*/
#include <bits/stdc++.h>
using namespace std;


int main(){
    // fastio(3);

    long long n=0, m=0, x_max=0, y_max=0, x_min=0, y_min=0;
    long long x[4], y[4], ans=0;
    y_max = x_max = LONG_MIN;
    y_min = x_min = LONG_MAX;
    cin >> n;

    for(int i = 0; i < n; i++){
        cin >> x[i] >> y[i];
        if(x[i] > x_max)
            x_max = x[i];
        if(x[i] < x_min)
            x_min = x[i];
        if(y[i] > y_max)
            y_max = y[i];
        if(y[i] < y_min)
            y_min = y[i];
    }
    if(n == 1)
        ans = -1;
    else if(n == 2){
        if((x[0] == x[1]) || (y[1] == y[0]))
            ans = -1;
        else
            ans = abs(x[0]-x[1]) * abs(y[1]-y[0]);
    }
    else
        ans = abs(x_max-x_min) * abs(y_max-y_min);
    if(ans == 0)
        cout << "-1\n";
    else
        cout << ans << '\n';
}
