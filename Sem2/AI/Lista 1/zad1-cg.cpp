// zda 1 codingame edition
#include <bits/stdc++.h>
using namespace std;
#define WHITE true
#define BLACK false
const int L = 8;
const int N = 4*(L*L)*(L*L)*(L*L) + 5;
bool vis[N];
int father[N];         // table of parents to recreate path
ofstream output;

class Point{
    public:
        int row, col;
        Point(int _row, int _col) : row(_row), col(_col) {}

    bool operator== (Point p){
        if(p.row == row && p.col == col) return true;
        return false;
    }

    bool operator!= (Point p){
        return !(p == *this);
    } 

    Point operator+(Point p){
        row += p.row;
        col += p.col;
        return *this;
    }

    Point operator*(int a){
        row *= a;
        col *= a;
        return *this;
    }

    Point &operator--(){
        row--, col--;
        return *this;
    }

    Point operator--(int){
        Point temp = *this;
        operator--();
        return temp;
    }

    Point &operator++(){
        row++, col++;
        return *this;
    }

    Point operator++(int){
        Point temp = *this;
        operator++();
        return temp;
    }
};

// generate index from chess positions, when is odd then its black turn now
int generate_idx(bool turn, Point pos_b, Point pos_B, Point pos_C){
    pos_b--, pos_B--, pos_C--;
    return (pos_b.row + L*pos_b.col + L*L*pos_B.row + L*L*L*pos_B.col + L*L*L*L*pos_C.row + L*L*L*L*L*pos_C.col) * 2 + (turn ? 0 : 1);
}

class State{
    public:
        Point w, W, B;  // w - White tower, W - White king, B - Black king
        bool turn;      // WHITE/BLACK
        State(bool _turn, Point _b, Point _B, Point _C) : turn(_turn), w(_b), W(_B), B(_C) {}
};

int generate_idx(State state){return generate_idx(state.turn, state.w, state.W, state.B);}

int pow(int a, int w){int res = a; while(--w) res *= a; return res;}

State generate_state(int idx){
    int norm = (idx - ((idx % 2 == 0) ? 0 : 1)) / 2;
    Point w = {(norm%L), (norm%(pow(L, 2)))/L};
    Point W = {(norm%(pow(L, 3)))/pow(L, 2), (norm%(pow(L, 4)))/pow(L, 3)};
    Point B = {(norm%(pow(L, 5)))/pow(L, 4), (norm%(pow(L, 6)))/pow(L, 5)};
    State state((idx % 2 == 0), ++w, ++W, ++B);
    return state;
}

bool move_legal(Point p){       // check if move is legal - not out of boundaries
    if(p.row > 0 && p.col > 0 && p.row <= L && p.col <= L) return true;
    return false;
}

bool safe_position(State state){        // check if this position is beeing 'killed'
    if(state.turn == BLACK){
        // white king check
        for(int col = -1; col <= 1; col++) for(int row = -1; row <= 1; row++){
            Point p = {state.W.row + row, state.W.col + col};
            if(p == state.B) return false;
        }

        // white tower check
        vector<Point> moves = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};
        for(int m = 0; m < 4; m++){
            for(int i = 1; i <= L; i++){
                Point new_w(state.w.row + i*moves[m].row, state.w.col + i*moves[m].col);
                if(!move_legal(new_w) || state.W == new_w) break;
                if(state.B == new_w) return false;
            }
        }
    } else{
        // black king check
        for(int col = -1; col <= 1; col++) for(int row = -1; row <= 1; row++){
            Point p = {state.B.row + row, state.B.col + col};
            if(p == state.W) return false;
        }
    }

    return true;
}

vector<int> gen_possible_moves(int n){          // generate new possible states and return their indexes
    State state = generate_state(n);
    vector<int> res;

    if(state.turn == BLACK){
        // black king move
        for(int col = -1; col <= 1; col++) for(int row = -1; row <= 1; row++){
            Point new_C(state.B.row + row, state.B.col + col);
            State new_state = state; new_state.B = new_C;
            if(move_legal(new_C) && (col != 0 || row != 0) && safe_position(new_state) && state.W != new_C){
                int idx = generate_idx(!state.turn, state.w, state.W, new_C);
                res.push_back(idx);
            }
        }
    }

    else if(state.turn == WHITE){
        if(state.B == state.w) return res;  // black shouldn't kill
        // white tower move
        vector<Point> moves = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}};
        for(int m = 0; m < 4; m++){
            for(int i = 1; i <= L; i++){
                Point new_w(state.w.row + i*moves[m].row, state.w.col + i*moves[m].col);
                if(!move_legal(new_w) || state.B == new_w || state.W == new_w) break;
                int idx = generate_idx(!state.turn, new_w, state.W, state.B);
                res.push_back(idx);
            }
        }

        // white king move
        for(int col = -1; col <= 1; col++) for(int row = -1; row <= 1; row++){
            Point new_B(state.W.row + row, state.W.col + col);
            State new_state = state; new_state.W = new_B;
            if(move_legal(new_B) && (col != 0 || row != 0) && safe_position(new_state) && state.w != new_B && state.B != new_B){
                int idx = generate_idx(!state.turn, state.w, new_B, state.B);
                res.push_back(idx);
            }
        }
    }

    return res;
}

pair<int, int> bfs(int start_idx){      // {number of steps, end node idx}
    fill(vis, vis+N, false);
    father[start_idx] = start_idx;
    queue< pair<int, int> > q;
    q.push({start_idx, 1});

    while(!q.empty()){
        int now = q.front().first;
        int now_dist = q.front().second;
        q.pop();
        if(vis[now]) continue;
        vis[now] = true;

        vector<int> new_moves = gen_possible_moves(now);

        {
            State check_state = generate_state(now);
            // if its black turn and king is in danger and king cant run anywhere
            if(check_state.turn == BLACK && new_moves.size() == 0 && !safe_position(check_state)) return {now_dist, now};
        }


        for(int i : new_moves){
            if(!vis[i]) {
                father[i] = now;
                q.push({i, now_dist+1});
            }
        }
    }

    return {-1, 0};
}

string calc_move(State from, State to){
    string res = "";
    if(from.B != to.B){
        res += char(from.B.row + 'a' - 1); res += char(from.B.col + '0');
        res += char(to.B.row + 'a' - 1); res += char(to.B.col + '0');
        return res;
    } else if(from.W != to.W){
        res += char(from.W.row + 'a' - 1); res += char(from.W.col + '0');
        res += char(to.W.row + 'a' - 1); res += char(to.W.col + '0');
        return res;
    } else{
        res += char(from.w.row + 'a' - 1); res += char(from.w.col + '0');
        res += char(to.w.row + 'a' - 1); res += char(to.w.col + '0');
        return res;
    }
}


int main(){
    string turn; // Either black or white
    string W; // Position of the white king, e.g. a2
    string w; // Position of the white rook
    string B; // Position of the black king
    cin >> turn >> W >> w >> B; cin.ignore();

    //! row: a to h; col: 1 to 8; input sequence: white king, white tower, black king
    int starting_idx = generate_idx((turn[0] == 'w' ? WHITE : BLACK), {w[0]-'a'+1, w[1]-'0'}, {W[0]-'a'+1, W[1]-'0'}, {B[0]-'a'+1, B[1]-'0'});

    fill(vis, vis+N, false);
    pair<int, int> res = bfs(starting_idx);

    stack<int> indexes;
    int here = res.second;

    while(here != father[here]){
        indexes.push(here);
        here = father[here];
    }

    State last = generate_state(here);
    while(!indexes.empty()){
        here = indexes.top();
        indexes.pop();

        State here_state = generate_state(here);
        cout << calc_move(last, here_state) << ' ';

        last = here_state;
    }

    cout << '\n';
}
