#include <bits/stdc++.h>
using namespace std;

// find NIFS3
double calc_nifs3(double X, vector<double>& x0, vector<double>& y0, vector<double>& M){
    int k = 1;
    while(x0[k] < X) k++;

    return (1/(x0[k]-x0[k-1]))
            * (M[k-1]*pow(x0[k] - X, 3)/6.
            +  M[k] * pow(X - x0[k-1], 3)/6. 
            + (y0[k-1] - M[k-1]*pow(x0[k]-x0[k-1], 2)/6.)*(x0[k]-X)
            + (y0[k] - M[k]*pow(x0[k]-x0[k-1], 2)/6.)*(X-x0[k-1]));
}


// helper functions
double lambda(int k, vector<double>& x){
    return (x[k] - x[k-1])/(x[k+1] - x[k-1]);
}

double dk(int k, vector<double>& x, vector<double>& y){
    double t1 = (y[k+1] - y[k])/(x[k+1] - x[k]);
    double t2 = (y[k] - y[k-1])/(x[k] - x[k-1]);
    return 6 * (t1 - t2)/(x[k+1] - x[k-1]);
}

// find Mx, My (moments)
pair<vector<double>, vector<double>> calc_2_moments(vector<double>& t, vector<double>& x, vector<double>& y){
    int n = t.size()-1;
    vector<double> q, p, ux, uy;
    q.reserve(n), p.reserve(n), ux.reserve(n), uy.reserve(n);
    q.push_back(0), p.push_back(0), ux.push_back(0), uy.push_back(0);

    for(int k = 1; k <= n-1; k++){
        double lk = lambda(k, t);
        p.push_back(lk * q[k-1] + 2);
        q.push_back((lk-1)/p[k]);
        ux.push_back((dk(k, t, x) - lk * ux[k-1])/p[k]);
        uy.push_back((dk(k, t, y) - lk * uy[k-1])/p[k]);
    }

    vector<double> Mx(n+1), My(n+1);
    Mx[n] = My[n] = 0;
    Mx[n-1] = ux[n-1];
    My[n-1] = uy[n-1];
    for(int k = n-2; k >= 0; k--){
        Mx[k] = ux[k] + q[k] * Mx[k+1];
        My[k] = uy[k] + q[k] * My[k+1];
    }

    return {Mx, My};
}


// ------------------ main ------------------
int main(int argc, char** argv){        // filename, ([n])
    int M; cin >> M;
    int n = 0;
    vector<double> t;
    vector<double> x;
    vector<double> y;

    // ----- fill up x and y tables -----
    cin >> n;
    double a, b;
    for(int i = 0; i < n; i++){
        cin >> a >> b;
        x.push_back(a);
        y.push_back(b);
    }
    n -= 1;
    
    // ----- fill up t table -----
    for(int k = 0; k <= n; k++){
        t.push_back(k/(double)n);
    }

    // ----- take care of moments -----
    auto[Mx, My] = calc_2_moments(t, x, y);

    // ----- calculate NIFS3 -----
    for(double k = 0; k <= M; k += 1){
        cout << calc_nifs3(k/M, t, x, Mx) << ' ' << calc_nifs3(k/M, t, y, My) << '\n';
    }
}
