// g++ -o zad2 -std=c++17 -Ofast zad2.cpp; python .\validator3.py zad2 python .\zad2_parser.py
#include "bits/stdc++.h"
using namespace std;

#define cline_out if(false) cout
#define int short       // for space efficiency
const int32_t MAX_N = 30, FULL = (1 << MAX_N) - 1;

vector < vector < int > > row_values, col_values;
vector < vector < vector < int > > > row_domains, col_domains;
int ROWS, COLS;

void input() {
    ifstream io;
    io.open("zad2_cpp_input.txt");
    io >> ROWS >> COLS;
    row_values.resize(ROWS);
    for(int row = 0; row < ROWS; row++) {
        int k;  io >> k;
        row_values[row].resize(k);
        for(int j = 0; j < k; j++) {
            io >> row_values[row][j];
        }
    }
    col_values.resize(COLS);
    for(int col = 0; col < COLS; col++) {
        int k;  io >> k;
        col_values[col].resize(k);
        for(int j = 0; j < k; j++) {
            io >> col_values[col][j];
        }
    }
    row_domains.resize(ROWS);
    for(int row = 0; row < ROWS; row++) {
        int d;  io >> d;
        row_domains[row].resize(d);
        for(int i = 0; i < d; i++) {
            int k;  io >> k;
            row_domains[row][i].resize(k);
            for(int j = 0; j < k; j++) {
                io >> row_domains[row][i][j];
            }
        }
    }
    col_domains.resize(COLS);
    for(int col = 0; col < COLS; col++) {
        int d;  io >> d;
        col_domains[col].resize(d);
        for(int i = 0; i < d; i++) {
            int k;  io >> k;
            col_domains[col][i].resize(k);
            for(int j = 0; j < k; j++) {
                io >> col_domains[col][i][j];
            }
        }
    }
    io.close();
}

void output_domain(const vector < vector < vector < int > > >& domain) {
    for(auto d: domain) {
        cline_out << "[";
        for(auto p: d) {
            cline_out << "(";
            for(auto v: p) {
                cline_out << v << " ";
            }
            cline_out << ")";
        }
        cline_out << "] ";
    }
    cline_out << "\n";
}

// repairs the domain - removes all domains that are not compatible with the given colors
vector < vector < int > > fix(const vector < vector < int > >& domain, const vector < int >& values, int x, int y, bool value) {
    vector < vector < int > > new_domain;
    for(auto d: domain) {
        int32_t mask = 0;
        for(int i = 0; i < values.size(); i++) {
            for(int j = 0; j < values[i]; j++) {
                mask |= (1 << (d[i] + j));
            }
        }
        if(value == bool((mask & (1 << y)))) {
            new_domain.push_back(d);
        }
    }
    return new_domain;
}

// here we get our pixel color masks, which represent set colors for pixels
void fitting(int32_t& mask0, int32_t& mask1, const vector < vector < int > >& domain, const vector < int >& values, int lim) {
    int32_t MASK = (1 << lim) - 1;
    mask0 = mask1 = MASK;
    for(auto d: domain) {
        int pos = 0;
        for(int i = 0; i < values.size() + 1; i++) {
            int nxt_pos = lim, steps = 0;
            if(i < values.size()) {
                nxt_pos = d[i];
                steps = values[i];
            }
            while(pos < nxt_pos) { // 0
                mask1 &= MASK ^ (1 << pos);
                pos++;
            }
            while(pos < nxt_pos + steps) { // 1
                mask0 &= MASK ^ (1 << pos);
                pos++;
            }
        }
    }
}

// here we repair out domains, using the fitting function
bool inference(const vector < vector < vector < int > > >& domain1, vector < vector < vector < int > > >& domain2, const vector < vector < int > >& values1, const vector < vector < int > >& values2) {
    bool flag = false;
    for(int i = 0; i < values1.size(); i++) {
        int32_t mask0, mask1;
        fitting(mask0, mask1, domain1[i], values1[i], values2.size());
        for(int j = 0; j < values2.size(); j++) {
            vector < vector < int > > new_domain2;
            for(int p = 0; p < domain2[j].size(); p++) {
                int32_t mask = 0;
                for(int k = 0; k < values2[j].size(); k++) {
                    for(int l = 0; l < values2[j][k]; l++) {
                        mask |= (1 << (domain2[j][p][k] + l));
                        if(domain2[j][p][k] + l >= i) break;
                    }
                }
                bool flag1 = (mask0 & (1 << j)) && (mask & (1 << i));
                bool flag2 = (mask1 & (1 << j)) && !(mask & (1 << i));
                if(flag1 || flag2) {
                    flag = true;
                } else {
                    new_domain2.push_back(domain2[j][p]);
                }
            }
            domain2[j] = new_domain2;
        }
    }
    return flag;
}

void satisfy_constraints(vector < vector < vector < int > > >& row_domain, vector < vector < vector < int > > >& col_domain) {
    while(true) {
        bool flag1 = inference(row_domain, col_domain, row_values, col_values);
        bool flag2 = inference(col_domain, row_domain, col_values, row_values);
        if(!(flag1 || flag2)) return;
    }
}

int check_domain(const vector < vector < vector < int > > >& domain) {
    // returns -1 if there is an empty domain == no solution
    // returns  1 if solution has been found
    int flag = 1;
    for(auto d: domain) {
        if(d.size() == 0)   return -1;
        else if(d.size() != 1)  flag = 0;
    }
    return flag;
}

// exit function - prints the nonogram and quits the program
void build_nonogram(const vector < vector < vector < int > > >& domain, const vector < vector < int >  >& values) {
    vector < vector < int > > nonogram(ROWS, vector < int > (COLS, 0));
    ofstream output;
    output.open("zad_output.txt");
    for(int i = 0; i < ROWS; i++) {
        for(int j = 0; j < values[i].size(); j++) {
            for(int k = 0; k < values[i][j]; k++) {
                nonogram[i][domain[i][0][j] + k] = 1;
            }
        }
    }
    for(int i = 0; i < ROWS; i++) {
        for(int j = 0; j < COLS; j++) {
            output << (nonogram[i][j] == 1 ? '#' : '.');
            cline_out << (nonogram[i][j] == 1 ? '#' : '.');
        }
        output << "\n";
        cline_out << "\n";
    }
    output.close();
    exit(0);
}


// main loop, solves the nonogram for given pixel color; we achieve backtracking with recursion
void solve_nonogram(vector < vector < vector < int > > > row_domain,vector < vector < vector < int > > > col_domain, int row, int col, int value) {
    vector < vector < int > > new_row_domain = fix(row_domain[row], row_values[row], row, col, value);
    row_domain[row] = new_row_domain;
    vector < vector < int > > new_col_domain = fix(col_domain[col], col_values[col], col, row, value);
    col_domain[col] = new_col_domain;
    int flag = check_domain(row_domain);
    if(flag == -1)  return;
    flag = min(flag, check_domain(col_domain));
    if(flag == -1) return;
    if(flag == +1) build_nonogram(row_domain, row_values);
    col++;
    // backtracking - try all possibilities
    for(; row < ROWS; row++) {
        for(; col < COLS; col++) {
            solve_nonogram(row_domain, col_domain, row, col, 1);
            vector < vector < int > > cnt_row_domain = fix(row_domain[row], row_values[row], row, col, 0);
            row_domain[row] = cnt_row_domain;
            vector < vector < int > > cnt_col_domain = fix(col_domain[col], col_values[col], col, row, 0);
            col_domain[col] = cnt_col_domain;
            satisfy_constraints(row_domain, col_domain);
            int cnt_flag = min(check_domain(row_domain), check_domain(col_domain));
            if(cnt_flag == -1) return;
            if(cnt_flag == +1) build_nonogram(row_domain, row_values);
        }
        col = 0;
    }
}


int32_t main() {
    ios_base::sync_with_stdio(0); cin.tie(0); cout.tie(0);
    input();
    solve_nonogram(row_domains, col_domains, 0, 0, 1);
    solve_nonogram(row_domains, col_domains, 0, 0, 0);
}
