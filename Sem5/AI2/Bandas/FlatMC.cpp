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
const Point DIR_POINT[] = {Point(0, -1), Point(0, 1), Point(-1, 0), Point(1, 0)};
const string DIRECTIONS[] = {"UP", "DOWN", "LEFT", "RIGHT"};
const DIR PERPENDICULAR[] = {R, L, U, D};
const DIR OPPOSITE[] = {D, U, R, L};

const char EMPTY = '-', HOLE = 'x';
const int MAX_TURNS = 200;
Timer timer;

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


class FlatMonteCarloAgent {
    int my_id;

public:
    FlatMonteCarloAgent(int my_id) : my_id(my_id) {}

    // simulate to the end, return reward
    int simulate(GameState& state) {
        while(!state.is_terminal()) {
            DIR dir = (DIR)(rand() % 4);
            state.apply_move(dir);
        }
        return state.eval();
    }

    string choose_best_move(const GameState& state, int time_limit = 90) {
        vector<double> rewards(4, 0);
        vector<int> counts(4, 0);

        timer.reset();
        while(timer.elapsed() < time_limit) {
            GameState state_copy = state;
            DIR dir = (DIR)(rand() % 4);
            state_copy.apply_move(dir);
            int reward = simulate(state_copy);
            rewards[dir] += reward;
            counts[dir]++;
        }


        double best_avg = -1e9;
        int best_direction = 0;

        for(int dir = 0; dir < 4; dir++) {
            if(counts[dir] > 0) {
                double avg = rewards[dir] / counts[dir];
                if(avg > best_avg) {
                    best_avg = avg;
                    best_direction = dir;
                }
            }
        }

        return DIRECTIONS[best_direction];
    }
};

int main() {
    // ios_base::sync_with_stdio(false);
    // cin.tie(0);

    srand(time(0));

    char my_id;
    cin >> my_id; cin.ignore();
    int height, width;
    cin >> height >> width; cin.ignore();

    GameState state(my_id, vector<string>(height, string(width, ' ')));
    FlatMonteCarloAgent agent(my_id);

    while(true) {
        for(int i = 0; i < height; i++) {
            for(int j = 0; j < width; j++) {
                cin >> state.grid[i][j];
            }
        }
        state.recalc_pawns();

        string best_move = agent.choose_best_move(state, 95);
        cout << best_move << endl;
    }

    return 0;
}
