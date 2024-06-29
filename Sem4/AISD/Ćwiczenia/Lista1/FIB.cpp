#include <bits/stdc++.h>
using namespace std;
#define ll long long

class M2x2 {
public:
    ll M[2][2];

    M2x2() : M2x2(0, 0, 0, 0) {}
    M2x2(ll a, ll b, ll c, ll d) {
        M[0][0] = a, M[0][1] = b, M[1][0] = c, M[1][1] = d;
    }

    M2x2 pow_mod(ll pow, ll mod){
        if(pow == 0) return M2x2(1, 0, 0, 1);
        if(pow == 1) return *this % mod;

        M2x2 r = this->pow_mod(pow/2, mod);
        if(pow%2) return (*this * ((r*r) % mod)) % mod;
        return (r*r) % mod;
    }

    M2x2 operator+(M2x2 m2) {
        return M2x2(M[0][0] + m2.M[0][0], M[0][1] + m2.M[0][1], M[1][0] + m2.M[1][0], M[1][1] + m2.M[1][1]);
    }

    M2x2 operator*(M2x2 m2) {
        return M2x2(M[0][0]*m2.M[0][0] + M[0][1]*m2.M[1][0], M[0][0]*m2.M[0][1] + M[0][1]*m2.M[1][1],
                    M[1][0]*m2.M[0][0] + M[1][1]*m2.M[1][0], M[1][0]*m2.M[0][1] + M[1][1]*m2.M[1][1]);
    }

    M2x2 operator%(ll m) {
        return M2x2(M[0][0]%m, M[0][1]%m, M[1][0]%m, M[1][1]%m);
    }

    friend ostream& operator<<(ostream& os, const M2x2& m) {
        os << m.M[0][0] << ' ' << m.M[0][1] << '\n' << m.M[1][0] << ' ' << m.M[1][1];
        return os;
    }
};


int main(){
    ios_base::sync_with_stdio(false);
    cin.tie(0);

    int t; cin >> t;
    while(t--) {
        int n, mod; cin >> n >> mod;

        M2x2 Mfib(1, 1, 1, 0);
        M2x2 Mfibn = Mfib.pow_mod(n, mod);
        cout << Mfibn.M[1][0] << '\n';
    }

}
