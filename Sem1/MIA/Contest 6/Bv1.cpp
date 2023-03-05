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


void mult_matrix(LLI a[2][2], LLI b[2][2], LLI w[2][2]){
    LLI temp[2][2];
    temp[0][0] = w[0][0]; temp[1][0] = w[1][0]; temp[0][1] = w[0][1]; temp[1][1] = w[1][1];
    //A = AA + BC                                       //  [A, B]
    temp[0][0] = a[0][0]*b[0][0]%MOD + a[1][0]*b[0][1]%MOD;        //  [C, D]
    //B = AB + BD
    temp[1][0] = a[0][0]*b[1][0]%MOD + a[1][0]*b[1][1]%MOD;        //  [A, B]
    //C = CA + DC                                       //  [C, D]
    temp[0][1] = a[0][1]*b[0][0]%MOD + a[1][1]*b[0][1]%MOD;
    //D = CB + DD                                       //  [AA+BC, AB+BD]
    temp[1][1] = a[0][1]*b[1][0]%MOD + a[1][1]*b[1][1]%MOD;        //  [CA+DC, CB+DD]

    w[0][0] = temp[0][0]%MOD; w[1][0] = temp[1][0]%MOD; w[0][1] = temp[0][1]%MOD; w[1][1] = temp[1][1]%MOD;
}

void pot(LLI a[2][2], LLI n, LLI w[2][2]){
    LLI copy[2][2];
    copy[0][0] = a[0][0]%MOD; copy[1][0] = a[1][0]%MOD; copy[0][1] = a[0][1]%MOD; copy[1][1] = a[1][1]%MOD;
    while(n > 0){
        if(n%2 == 0){
            n /= 2;
            mult_matrix(copy, copy, copy);
        }
        else{
            n--;
            mult_matrix(a, copy, w);
            a = w;
            n /= 2;
            mult_matrix(copy, copy, copy);
        }
    }
}

LLI fib(LLI n){
    if(n == -1){
        return 0;
    }

    LLI w[2][2], a[2][2];
    a[0][0] = 1; a[1][0] = 1; a[0][1] = 1; a[1][1] = 0;
    w[0][0] = 1; w[1][0] = 1; w[0][1] = 1; w[1][1] = 1;
    pot(a, n, w);
    return w[1][0];
}

int main(){
    string message; cin >> message;

    LLI possibilities = 1, cnt = 0;
    FOR(i, 1, message.size()){
        if((message[i] == 'u' && message[i-1] == 'u') || (message[i] == 'n' && message[i-1] == 'n')){
            cnt++;
        } else if(message[i-1] == 'u' || message[i-1] == 'n'){
            // cout << "summing up [n/poss/secposs]: " << n_at_end << ' ' << possibilities << ' ' << section_possibilities << '\n';
            possibilities *= fib(cnt+1);
            possibilities %= MOD;
            cnt = 0;
        }

        if(message[i] == 'm' || message[i] == 'w'){
            possibilities = 0;
            break;
        }
    }
    possibilities *= fib(cnt+1);
    possibilities %= MOD;

    // cout << '\n';
    cout << possibilities << '\n';
}