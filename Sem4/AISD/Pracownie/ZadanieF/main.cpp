#include <bits/stdc++.h>
using namespace std;
#define cerr if(0) cout

const int L_PAT = 201;
const int L_TAB = 2e3+1;

const int P = 1e4+7;
const int cell_arity = 'z'-'a'+1;

string pat[L_PAT];      // [row][col]
string tab[L_TAB];
int pat_rows, pat_cols, tab_rows, tab_cols;

int pat_col_hash[L_PAT], pat_col_kmp[L_PAT], pat_kmp_pos;
//? TCHP - row position in columns, TCH - hashes of columns
int tab_col_hash_pos, tab_col_hash[L_TAB];

/*
! hashing concept:
* we start with computing hash for each column (for rows in range)
the higher cell the bigger arity multiplier (so we cut off at the top)
* then we make KMP on those columns
*/


// ---------- HASH INIT -----------
int calc_col_hash(const int from, const int to, const int col, const string tab[]) {
    int res = 0;
    for(int i = from; i < to; i++) {
        res = (cell_arity*res % P + tab[i][col]) % P;
    }
    return res;
}


void init_hashes() {
    for(int i = 0; i < pat_cols; i++) {
        pat_col_hash[i] = calc_col_hash(0, pat_rows, i, pat);
    }

    for(int i = 0; i < tab_cols; i++) {
        tab_col_hash[i] = calc_col_hash(0, pat_rows, i, tab);
    }
    tab_col_hash_pos = 0;
}

// ----------------- HASH MODIFIERS --------------------
void move_all_cols_hash_down() {
    const int h = (long long)pow(cell_arity, pat_rows-1) % P;

    for(int col = 0; col < tab_cols; col++) {
        tab_col_hash[col] = 
            ((cell_arity * (tab_col_hash[col] - (tab[tab_col_hash_pos][col] * h)%P+P) % P) % P + 
            tab[tab_col_hash_pos+pat_rows][col]) % P;
    }
    
    tab_col_hash_pos++;
}

// ---------------- KMP ----------------
// todo kmp on column hashes
void calc_pat_kmp() {
    // todo calc kmp
    pat_kmp_pos = 0;
}

bool test_col_kmp(int pos) {

}

// ----------------------------------------

bool check(const int row, const int col) {
    // todo run kmp check here
    // if(pat_hash != tab_hash) return false;

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
    for(int i = 0; i < tab_cols; i++) 
        cerr << tab_col_hash[i] << ' ' ;
    cerr << "\n------------\n\n";

    int cnt = 0;
    for(int row = 0; row <= tab_rows-pat_rows; row++) {
        for(int col = 0; col <= tab_cols-pat_cols; col++) {
            for(int i = col; i < col+pat_cols; i++) 
                cerr << tab_col_hash[i] << ' ' ;
            cerr << "-----------\n";

            if(check(row, col)) 
                cnt++;
        }
        cerr << "---DOWNSHIFT---\n";

        move_all_cols_hash_down();
        pat_kmp_pos = 0;
    }

    cout << cnt << '\n';
}