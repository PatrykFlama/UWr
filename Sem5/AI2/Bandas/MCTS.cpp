// #pragma GCC optimize("Ofast,inline,tracer")
// #pragma GCC optimize("unroll-loops,vpt,split-loops,unswitch-loops,omit-frame-pointer,inline")
// #pragma GCC option("march=native","tune=native","no-zero-upper")            //Enable AVX
// #pragma GCC target("arch=haswell,tune=haswell")
// #pragma GCC target("aes,abm,align-stringops,avx,avx2,bmi,bmi2,crc32,cx16,f16c,fma,fsgsbase,fxsr,hle,ieee-fp,lzcnt,mmx,movbe,mwait,pclmul,popcnt,rdrnd,sahf,sse,sse2,sse3,sse4,sse4.1,sse4.2,ssse3,xsave,xsaveopt")
// #include <x86intrin.h>                                                      //AVX/SSE Extensions


#include <bits/stdc++.h>
#include <chrono>
#include <thread>
using namespace std;

#define DEBUG 1
#define cerr if(DEBUG) cerr

/* #region --- HELPERS ---- */
// cout pair
template <typename T, typename U>
ostream& operator<<(ostream& os, const pair<T, U>& p) {
    os << "(" << p.first << ", " << p.second << ")";
    return os;
}

// hash pair
namespace std {
    template <typename T, typename U>
    struct hash<pair<T, U>> {
        size_t operator()(const pair<T, U>& p) const {
            return hash<T>()(p.first) ^ hash<U>()(p.second);
        }
    };
}


//? returns random number from 0 to 1
double random_uniform() {
    return static_cast<double>(rand()) / (double)RAND_MAX;
}
/* #endregion */

/* #region --- HELPER CLASSES------- */
class Timer {
public:
    chrono::time_point<chrono::high_resolution_clock> start_time;
    Timer() : start_time(chrono::high_resolution_clock::now()) {}

    void reset() {
        start_time = chrono::high_resolution_clock::now();
    }

    int64_t elapsed() {
        return chrono::duration_cast<chrono::milliseconds>(chrono::high_resolution_clock::now() - start_time).count();
    }
};


class Point {
public:
    int x, y;
    Point(int x, int y) : x(x), y(y) {}
    Point() : x(0), y(0) {}
    Point(const Point& p) : x(p.x), y(p.y) {}
    Point& operator=(const Point& p) {
        x = p.x;
        y = p.y;
        return *this;
    }

    Point operator+(const Point& p) const {
        return Point(x + p.x, y + p.y);
    }

    bool operator==(const Point& p) const {
        return x == p.x && y == p.y;
    }
    bool operator!=(const Point& p) const {
        return x != p.x || y != p.y;
    }

    bool operator<(const Point& p) const {
        return x < p.x ||(x == p.x && y < p.y);
    }

    int int_dist(const Point& p) const {
        return(x - p.x) *(x - p.x) +(y - p.y) *(y - p.y);
    }

    int manhattan_dist(const Point& p) const {
        return abs(x - p.x) + abs(y - p.y);
    }

    double dist(const Point& p) const {
        return sqrt(int_dist(p));
    }

    friend ostream& operator<<(ostream& os, const Point &p) {
        os << p.x << " " << p.y;
        return os;
    }
};

namespace std {
    template <>
    struct hash<Point> {
        size_t operator()(const Point& p) const {
            return hash<int>()(p.x) ^ hash<int>()(p.y);
        }
    };
}

/* #endregion */


enum DIR {U, D, L, R};
const string DIRECTIONS[] = {"UP", "DOWN", "LEFT", "RIGHT"};
const DIR PERPENDICULAR[] = {R, L, U, D};
const DIR OPPOSITE[] = {D, U, R, L};

const char EMPTY = '-', HOLE = 'x';
const int MAX_TURNS = 200;
Timer timer;

// MCTS constants
const double EXPLORATION_CONSTANT = 2*sqrt(2);
constexpr int MAX_ACTIONS = 4;
constexpr int MAX_TREE_SIZE = 10000;


class GameState {
public:
    const int height, width;
    int row_from, row_to, col_from, col_to;
    char my_id, opp_id, main_player;
    int turn;
    vector<string> grid;
    int my_pawns = 0, opp_pawns = 0;

    GameState(char my_id, const vector<string>& grid)
        : my_id(my_id), opp_id(my_id == '1' ? '0' : '1'), main_player(my_id), grid(grid), 
          height(grid.size()), width(grid[0].size()),
          row_from(0), row_to(height-1), col_from(0), col_to(width-1),
          turn(0) {
    }

    GameState& operator=(const GameState& other) {
        my_id = other.my_id;
        opp_id = other.opp_id;
        main_player = other.main_player;
        turn = other.turn;
        grid = other.grid;
        my_pawns = other.my_pawns;
        opp_pawns = other.opp_pawns;
        return *this;
    }

    bool operator==(const GameState& other) const {
        return grid == other.grid;
    }

    void recalc_pawns() {
        my_pawns = opp_pawns = 0;
        for(const auto& row : grid) {
            for(char cell : row) {
                if(cell == my_id) my_pawns++;
                else if(cell == opp_id) opp_pawns++;
            }
        }
    }

    void swap_players() {
        swap(my_id, opp_id);
        swap(my_pawns, opp_pawns);
    }

    int remove_empty_borders(int from, int step, bool is_row) {
        int cnt_removed = 0;

        if(is_row) {
            bool to_erase = true;
            for(int row = from; row_from <= row && row <= row_to; row += step) {
                for(int col = col_from; col <= col_to; col++) {
                    if(grid[row][col] != EMPTY) {
                        to_erase = false;
                        break;
                    }
                }

                if(!to_erase) break;

                cnt_removed++;
                // for(int row = from; row_from <= row && row <= row_to; row += step) {
                for(int col = col_from; col <= col_to; col++) {
                    grid[row][col] = HOLE;
                }
            }
        } else {
            bool to_erase = true;
            for(int col = from; col_from <= col && col <= col_to; col += step) {
                for(int row = row_from; row <= row_to; row++) {
                    if(grid[row][col] != EMPTY) {
                        to_erase = false;
                        break;
                    }
                }

                if(!to_erase) break;

                cnt_removed++;
                // for(int col = from; col_from <= col && col <= col_to; col += step) {
                for(int row = row_from; row <= row_to; row++) {
                    grid[row][col] = HOLE;
                }
            }
        }

        return cnt_removed;
    }

    void apply_move(const DIR dir) {
        // cerr << "Applying move " << DIRECTIONS[dir] << " with " << my_id << '\n';

        // push pawns
        if(dir == U || dir == D) {
            // iterate over columns, then rows
            for(int col = col_from; col <= col_to; col++) {
                char picked = EMPTY;

                int row_dir = (dir == U ? -1 : 1);
                int row = (dir == U ? row_to : row_from);

                for(; row >= row_from && row <= row_to; row += row_dir) {
                    const char cell = grid[row][col];

                    if(picked == EMPTY && cell == opp_id) {
                        continue;
                    }

                    grid[row][col] = picked;
                    picked = cell;
                }

                // piece that falls of the board
                if(picked == my_id) my_pawns--;
                else if(picked == opp_id) opp_pawns--;
            }
        } else {    // L or R
            // iterate over rows, then columns
            for(int row = row_from; row <= row_to; row++) {
                char picked = EMPTY;

                int col_dir = (dir == L ? -1 : 1);
                int col = (dir == L ? col_to : col_from);

                for(; col >= col_from && col <= col_to; col += col_dir) {
                    const char cell = grid[row][col];

                    if(picked == EMPTY && cell == opp_id) {
                        continue;
                    }

                    grid[row][col] = picked;
                    picked = cell;
                }

                // piece that falls off the board
                if(picked == my_id) my_pawns--;
                else if(picked == opp_id) opp_pawns--;
            }
        }

        // remove empty border rows and columns
        // cerr << "row col from to before: " << row_from << ' ' << row_to << ' ' << col_from << ' ' << col_to << '\n';
        row_from += remove_empty_borders(row_from, 1, true);
        row_to -= remove_empty_borders(row_to, -1, true);
        col_from += remove_empty_borders(col_from, 1, false);
        col_to -= remove_empty_borders(col_to, -1, false);
        // cerr << "row col from to after: " << row_from << ' ' << row_to << ' ' << col_from << ' ' << col_to << '\n';

        turn++;
        swap_players();

        // deb_print_grid();
        // cerr << '\n';
    }

    int eval() {
        if(main_player != my_id) {
            swap_players();
        }

        if(my_pawns == 0 && opp_pawns != 0) {
            return -(height * width);
        } else if(opp_pawns == 0) {
            return (height * width);
        } else {
            // return (my_pawns > opp_pawns ? 1 : (my_pawns < opp_pawns ? -1 : 0));
            return my_pawns - opp_pawns;
        }
    }

    bool is_terminal() const {
        return turn >= MAX_TURNS || my_pawns == 0 || opp_pawns == 0;
    }


    void deb_print_grid() {
        for(const auto& row : grid) {
            cerr << row << '\n';
        }
    }

    friend ostream& operator<<(ostream& os, const GameState& state) {
        os << "Main player: " << state.main_player << ", My id: " << state.my_id << ", Opp id: " << state.opp_id << '\n';
        os << "My pawns: " << state.my_pawns << ", Opp pawns: " << state.opp_pawns << '\n';
        for(const auto& row : state.grid) {
            os << row << '\n';
        }
        return os;
    }
};


// ----------------- MCTS -----------------
struct Node {
    double reward = 0;
    int visits = 0;
    int parentId = 0;
    int childrenNum = 0;
    int childrenId[MAX_ACTIONS];    // 0 <=> not initialized
    GameState state;

    Node(int parentId, const GameState state)
        : parentId(parentId), state(state) {
        for(int i = 0; i < MAX_ACTIONS; i++) {
            this->childrenId[i] = 0;
        }
    }
};

// Monte Carlo Tree Search Implementation
class MCTS {
public:
    Node* root;
    int rootId = 0;
    Node* tree[MAX_TREE_SIZE];
    int treeSize = 1;

    int selectedStates[MAX_TURNS + 1];
    int selectedStatesNum = 0;

    MCTS(GameState state) {
        tree[rootId] = new Node(0, state);
        root = tree[rootId];
    }

    void opp_move(GameState state) {
        // cerr << "Opponent move\n";
        for(int i = 0; i < MAX_ACTIONS; i++) {
            if(root->childrenId[i] == 0) {
                expand(rootId);
            }

            if(tree[root->childrenId[i]]->state == state) {
                rootId = root->childrenId[i];
                root = tree[rootId];

                // cerr << root->state << '\n';

                // cerr << "With children:\n";
                // for(int j = 0; j < MAX_ACTIONS; j++) {
                //     if(root->childrenId[j] != 0) {
                //         cerr << DIRECTIONS[j] << " " << root->childrenId[j] << "\n" << tree[root->childrenId[j]]->state << '\n';
                //     } else {
                //         cerr << DIRECTIONS[j] << " " << root->childrenId[j] << '\n';
                //     }
                // }
                return;
            }
        }
    }

    // ----- MCTS funcitons ------
    int tree_policy(int stateId) {
        while(!tree[stateId]->state.is_terminal()) {
            if(tree[stateId]->childrenNum < MAX_ACTIONS) {
                return expand(stateId);
            } else {
                stateId = best_child(stateId, EXPLORATION_CONSTANT);
            }
        }

        return stateId;     // ERR - should not happen
    }

    static inline double UCB1(int reward, int visits, int parent_visits, int c) {
        return reward / (double)visits + c * sqrt(log(parent_visits) / visits);
    }

    int best_child(const int stateId, const double c) const {
        double best_score = -1e9;
        int best_action = 0;

        for(int action = 0; action < MAX_ACTIONS; action++) {
            const int childId = tree[stateId]->childrenId[action];
            if(childId == 0) continue;

            const double score = UCB1(tree[childId]->reward, tree[childId]->visits, tree[stateId]->visits, c);
            if(score > best_score) {
                best_score = score;
                best_action = action;
            }
        }

        return tree[stateId]->childrenId[best_action];
    }

    int expand(const int stateId) {
        const int action = tree[stateId]->childrenNum;
        tree[stateId]->childrenNum++;
        // cerr << "Expanding state " << stateId << " with action " << DIRECTIONS[action] << '\n';

        tree[treeSize] = new Node(stateId, tree[stateId]->state);
        tree[treeSize]->state.apply_move((DIR)action);
        tree[stateId]->childrenId[action] = treeSize;

        return treeSize++;
    }

    int default_policy(const int stateId) {
        GameState state = tree[stateId]->state;
        while(!state.is_terminal()) {
            const DIR dir = (DIR)(rand() % MAX_ACTIONS);
            state.apply_move(dir);
        }

        return state.eval();
    }

    void backup(int stateId, int reward) {
        while(stateId != 0) {
            tree[stateId]->reward += reward;
            tree[stateId]->visits++;
            stateId = tree[stateId]->parentId;
        }
    }

    // ----- choosing best move -----
    DIR child_to_action(const int parentId, const int childId) const {
        for(int action = 0; action < MAX_ACTIONS; action++) {
            if(tree[parentId]->childrenId[action] == childId) {
                return (DIR)action;
            }
        }
        return (DIR)0;   // ERR - should never happen
    }

    int robust_child(const int stateId) const { // choose the most visited child
        cerr << "Choosing robust child\n";
        double best_score = -1e9;
        int best_action = 0;

        for(int action = 0; action < MAX_ACTIONS; action++) {
            cerr << "Action: " << DIRECTIONS[action] << " | " << tree[stateId]->childrenId[action] << ", " << tree[tree[stateId]->childrenId[action]]->reward << " / " << tree[tree[stateId]->childrenId[action]]->visits << '\n';
            // cerr << tree[tree[stateId]->childrenId[action]]->state << '\n';
            const int childId = tree[stateId]->childrenId[action];
            if(childId == 0) continue;

            const double score = tree[childId]->reward / tree[childId]->visits;
            if(score > best_score) {
                best_score = score;
                best_action = action;
            }
        }

        return tree[stateId]->childrenId[best_action];
    }

    string choose_best_move(int time_limit) {
        timer.reset();
        while(timer.elapsed() < time_limit) {
            const int stateId = tree_policy(rootId);    // walk down the existing tree
            const int reward = default_policy(stateId); // simulate the rest of the game
            backup(stateId, reward);                    // update the tree  
        }

        const int best_child_id = robust_child(rootId);
        const DIR best_action = child_to_action(rootId, best_child_id);

        rootId = best_child_id;
        root = tree[best_child_id];

        return DIRECTIONS[best_action];
    }
};



int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(0);

    srand(time(0));

    char my_id;
    cin >> my_id; cin.ignore();
    int height, width;
    cin >> height >> width; cin.ignore();

    GameState state(my_id, vector<string>(height, string(width, ' ')));
    MCTS *mcts = nullptr;

    while(true) {
        for(int i = 0; i < height; i++) {
            for(int j = 0; j < width; j++) {
                cin >> state.grid[i][j];
            }
        }
        state.recalc_pawns();

        if(mcts == nullptr) {
            mcts = new MCTS(state);
        } else {
            mcts->opp_move(state);
        }

        string best_move = mcts->choose_best_move(90);
        cout << best_move << endl;
    }

    return 0;
}
