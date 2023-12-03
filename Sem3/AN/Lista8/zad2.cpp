#include <bits/stdc++.h>
using namespace std;
#define cerr if(0) cerr


class Poly{
    vector<double> a;

public:
    Poly (vector<double> a) : a(a) {}
    Poly (initializer_list<double> a) : a(a) {}
    Poly (string s) {
        stringstream ss(s);
        double x;
        while (ss >> x) {
            a.push_back(x);
        }
    }

    double eval(double x) {
        double res = 0;
        for (int i = 0; i < a.size(); i++) {
            res *= x;
            res += a[i];
        }
        return res;
    }

    Poly derivative(int n = 1) {
        vector<double> res = a;

        while(n){
            vector<double> temp;
            for (int i = 0; i < res.size()-1; i++) {
                temp.push_back(res[i] * (res.size()-i-1));
            }
            res = temp;
            n--;
        }

        return Poly(res);
    }

    friend ostream &operator<<(ostream &os, const Poly &p) {
        for (int i = 0; i < p.a.size()-1; i++) {
            os << p.a[i] << "x^" << p.a.size()-i-1 << " + ";
        }
        os << p.a.back();
        return os;
    }
};

class NIFS3{
    vector<Poly> s;
    vector<double> x0;

public:
    NIFS3 (vector<Poly> s, vector<double> x0) : s(s), x0(x0) {
        if (s.size() != x0.size()-1) throw "Wrong sizes";
    }
    NIFS3 (initializer_list<Poly> s, initializer_list<double> x0) : s(s), x0(x0) {
        if (s.size() != x0.size()-1) throw "Wrong sizes";
    }

    bool check_if_nifs3() {
        for (int i = 1; i < x0.size()-1; i++){
            cerr << "\nchecking " << i << " at " << x0[i] <<'\n';
            cerr << s[i-1] << " -> " << s[i-1].eval(x0[i]) << ' ' << s[i] << " -> " << s[i].eval(x0[i]) << '\n' <<
                    s[i-1].derivative() << " -> " << s[i-1].derivative().eval(x0[i]) << ' ' << s[i].derivative() << " -> " << s[i].derivative().eval(x0[i]) << '\n' <<
                    s[i-1].derivative(2) << " -> " << s[i-1].derivative(2).eval(x0[i]) << ' ' << s[i].derivative(2) << " -> " << s[i].derivative(2).eval(x0[i]) << '\n';

            if (s[i-1].eval(x0[i]) != s[i].eval(x0[i])) return false;
            if (s[i-1].derivative().eval(x0[i]) != s[i].derivative().eval(x0[i])) return false;
            if (s[i-1].derivative(2).eval(x0[i]) != s[i].derivative(2).eval(x0[i])) return false;
        }
        if (s[0].derivative(2).eval(x0[0]) != 0 || s.back().derivative(2).eval(x0.back()) != 0) return false;
        return true;
    }

    vector<pair<double, double>> values_at_x0(int derivative = 0) {
        vector<pair<double, double>> res;
        for (int i = 1; i < x0.size()-1; i++) {
            res.push_back({x0[i], s[i].derivative(derivative).eval(x0[i])});
        }
        return res;
    }

    void print_values(int derivative = 0) {
        cout << "\nValues at x0:\n";
        cout << "F: \n";
        for(auto i : values_at_x0(0)) cout << i.first << ' ' << i.second << '\n';

        cout << "First derivative: \n";
        for(auto i : values_at_x0(1)) cout << i.first << ' ' << i.second << '\n';

        cout << "Second derivative: \n";
        cout << x0[0] << ' ' << s[0].derivative(2).eval(x0[0]) << '\n';
        for(auto i : values_at_x0(2)) cout << i.first << ' ' << i.second << '\n';
        cout << x0.back() << ' ' << s.back().derivative(2).eval(x0.back()) << '\n';
    }
};


int main(){
    NIFS3 nifs3(
        {
            {24, 216, 500, 328},
            {-17, -30, 8, 0},
            {22, -30, 8, 0},
            {-6, 54, -76, 28}
        },
        {-3, -2, 0, 1, 3}
    );

    cout << "Given polynomial is " << (nifs3.check_if_nifs3() ? "" : "not ") << "a NIFS3\n";

    nifs3.print_values();
}
