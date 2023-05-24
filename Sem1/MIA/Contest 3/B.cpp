#define GET_MACRO(_1, _2, _3, _4, NAME, ...) NAME
#define _FOR3(i, a, n, inc) for(auto i = (a); (inc) > 0 ? i < (n) : i >= (n); i += (inc))
#define _FOR2(i, a, n) _FOR3(i, a, n, 1)
#define _FOR1(i, n) _FOR2(i, 0, n)
#define _FOR0(n) _FOR1(i, n)
#define FOR(...) GET_MACRO(__VA_ARGS__, _FOR3, _FOR2, _FOR1, _FOR0)(__VA_ARGS__)

#include <bits/stdc++.h>
using namespace std;
#define debug false
#define cerr if(debug) cout
#define ll long long


ll calc_zcy(ll n, ll p){
    string s = "";
    while(n){
        s += n%10 + '0';
        n /= 10;
    }
    
    string rev = s;
    reverse(rev.begin(), rev.end());
    s = rev+s;

    ll sum = 0;
    FOR(j, s.length()){
        sum = (sum*10+(s[j]-'0'))%p;
    }

    return sum;
}

int main(){
    ios_base::sync_with_stdio(false);
    cin.tie(0);
    cout.tie(0);

    ll k, p; cin >> k >> p;
    ll sum = 0;

    for(ll i = 1; i <= k; i++){
        sum = (sum+calc_zcy(i, p)) % p;
    }

    cout << sum << '\n';
}