#include <bits/stdc++.h>
using namespace std;
#define cerr if(0) cout

const int L_PAT = 201;
const int L_TAB = 2e3+1;

const int P = 1e4+7;
const int cell_arity = 'z'-'a'+1;
const int col_arity = cell_arity*L_PAT+1;

string pat[L_PAT];      // [row][col]
string tab[L_TAB];
int pat_rows, pat_cols, tab_rows, tab_cols;

int pat_col_hash[L_PAT], pat_hash;
//? TCHP - row position in columns, TCH - hashes of columns, THP - column position in tab, TH - table hash
int tab_col_hash_pos, tab_col_hash[L_TAB], tab_hash_pos, tab_hash;

/*
! hashing concept:
* we start with computing hash for each column (for rows in range)
the higher cell the bigger arity multiplier (so we cut off at the top)
* then we make second pass to calc hash of whole table
columns on left have bigger multiplier, so we cut them off first

we have 2 types of recalc:
* move columns 1 row down
* move table 1 column right

that means that we have:
* for cells alphabet from 'a' to 'z'
* for columns alphabet from 'a'*L_PAT to 'z'*L_PAT      //! yeah, well - that aint exactly true xD
*/


int calc_col_hash(const int from, const int to, const int col, const string tab[]) {
    int res = 0;
    for(int i = from; i < to; i++) {
        res = (cell_arity*res % P + tab[i][col]) % P;
    }
    return res;
}

// based on columns hash
int calc_tab_hash(const int from, const int to, const int tab[]) {
    int res = 0;
    for(int i = from; i < to; i++) {
        res = (col_arity*res % P + tab[i]) % P;
    }
    return res;
}

inline void refresh_tab_hash() {
    tab_hash = calc_tab_hash(0, pat_cols, tab_col_hash);
    tab_hash_pos = 0;
}

void init_hashes() {
    for(int i = 0; i < pat_cols; i++) {
        pat_col_hash[i] = calc_col_hash(0, pat_rows, i, pat);
    }
    pat_hash = calc_tab_hash(0, pat_cols, pat_col_hash);

    for(int i = 0; i < tab_cols; i++) {
        tab_col_hash[i] = calc_col_hash(0, pat_rows, i, tab);
    }
    tab_col_hash_pos = 0;
    refresh_tab_hash();
}

// -------------------------------------
void move_all_cols_hash_down() {
    const int h = (long long)pow(cell_arity, pat_rows-1) % P;

    for(int col = 0; col < tab_cols; col++) {
        tab_col_hash[col] = 
            ((cell_arity * (tab_col_hash[col] - (tab[tab_col_hash_pos][col] * h)%P+P) % P) % P + 
            tab[tab_col_hash_pos+pat_rows][col]) % P;
    }
    
    tab_col_hash_pos++;
}

void move_tab_right() {
    const int h = (long long)pow(col_arity, pat_cols-1) % P;
    tab_hash = ((col_arity * (tab_hash - (tab_col_hash[tab_hash_pos] * h)%P+P) % P) % P +
                tab_col_hash[tab_hash_pos+pat_cols]) % P;
    tab_hash_pos++;
}

// ----------------------------------------

bool check(const int row, const int col) {
    if(pat_hash != tab_hash) return false;

    int r = 0, tab_rows = 0;
    while(r < pat_rows) {
        tab_rows = 0;
        while(tab_rows < pat_cols) {
            if(tab[row+r][col+tab_rows] != pat[r][tab_rows])
                return false;
            ++tab_rows;
        }
        ++r;
    }
    return true;
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> pat_rows >> pat_cols >> tab_rows >> tab_cols;

    for(int i = 0; i < pat_rows; i++)
        cin >> pat[i];
    for(int i = 0; i < tab_rows; i++)
        cin >> tab[i];


    init_hashes();

    for(int i = 0; i < pat_cols; i++)
        cerr << pat_col_hash[i] << ' ';
    cerr << '\n' << pat_hash << '\n';
    for(int i = 0; i < tab_cols; i++) 
        cerr << tab_col_hash[i] << ' ' ;
    cerr << '\n' << tab_hash << '\n';
    cerr << "\n------------\n\n";

    int cnt = 0;
    for(int row = 0; row <= tab_rows-pat_rows; row++) {
        for(int col = 0; col <= tab_cols-pat_cols; col++) {
            for(int i = col; i < col+pat_cols; i++) 
                cerr << tab_col_hash[i] << ' ' ;
            cerr << '\n' << tab_hash << '\n';
            cerr << "-----------\n";

            if(check(row, col)) 
                cnt++;
            if(col < tab_cols-pat_cols)
                move_tab_right();
        }
        cerr << "---DOWNSHIFT---\n";

        move_all_cols_hash_down();
        refresh_tab_hash();
    }

    cout << cnt << '\n';
}