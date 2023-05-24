/* #region */
/* --- LIBRARIES --- */
#include <bits/stdc++.h>
using namespace std;

/* --- SUPERFOR --- */
#define GET_MACRO_FOR(_1, _2, _3, _4, NAME, ...) NAME
#define _FOR3(i, a, n, inc) for (auto i = (a); (inc) > 0 ? i < (n) : i >= (n); i += (inc))
#define _FOR2(i, a, n) _FOR3(i, a, n, 1)
#define _FOR1(i, n) _FOR2(i, 0, n)
#define _FOR0(n) _FOR1(i, n)
#define FOR(...) GET_MACRO_FOR(__VA_ARGS__, _FOR3, _FOR2, _FOR1, _FOR0)(__VA_ARGS__)    //? (/name/, //from//, to, ///inc///)

/* --- MISC --- */
#define cerr if (debug) cout
/* #endregion */
const long long MOD = 1e9+7;
#define LLI long long


LLI fib(LLI n){
    LLI a = 1 , b = 1;
    FOR(n){
        LLI temp = b;
        b = (a+b)%MOD;
        a = temp;
    }

    return b;
}

int main(){
    string message; cin >> message;
    bool error = 0;

    LLI possibilities = 1, cnt = 0;
    if(message[0] == 'm' || message[0] == 'w'){
        possibilities = 0;
        cout << 0 << '\n';
        return 0;
    }
    FOR(i, 1, message.size()){
        if((message[i] == 'u' && message[i-1] == 'u') || (message[i] == 'n' && message[i-1] == 'n')){
            cnt++;
        } else if(message[i-1] == 'u' || message[i-1] == 'n'){
            // cout << "summing up [n/poss/secposs]: " << n_at_end << ' ' << possibilities << ' ' << section_possibilities << '\n';
            possibilities *= fib(cnt);
            possibilities %= MOD;
            cnt = 0;
        }

        if(message[i] == 'm' || message[i] == 'w'){
            possibilities = 0;
            error = 1;
            break;
        }
    }
    possibilities *= fib(cnt);
    possibilities %= MOD;

    

    cout << possibilities << '\n';
}