#pragma GCC optimize("Ofast,inline,tracer")
#pragma GCC optimize("unroll-loops,vpt,split-loops,unswitch-loops,omit-frame-pointer,inline")
#pragma GCC option("march=native","tune=native","no-zero-upper")            //Enable AVX
#pragma GCC target("arch=haswell,tune=haswell")
#pragma GCC target("aes,abm,align-stringops,avx,avx2,bmi,bmi2,crc32,cx16,f16c,fma,fsgsbase,fxsr,hle,ieee-fp,lzcnt,mmx,movbe,mwait,pclmul,popcnt,rdrnd,sahf,sse,sse2,sse3,sse4,sse4.1,sse4.2,ssse3,xsave,xsaveopt")
#include <x86intrin.h>                                                      //AVX/SSE Extensions


#include <bits/stdc++.h>
#include <chrono>
#include <thread>
using namespace std;

#define cerr if(0) cerr


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

// ------- CONSTANTS -------
const int ROWS = 10, COLS = 19;
const char VOID = '#', PLATFORM = '.';
enum DIR {UP, RIGHT, DOWN, LEFT};
const char DIR_TO_CHAR[] = {'U', 'R', 'D', 'L'};
const short DIR_TO_SHORT_MASK[] = {0b0001, 0b0010, 0b0100, 0b1000};
// unordered_map<char, DIR> CHAR_TO_DIR = {{'U', UP}, {'R', RIGHT}, {'D', DOWN}, {'L', LEFT}};      // too slow
DIR CHAR_TO_DIR[CHAR_MAX];

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

    int dist(const Point& p) const {
        return (x - p.x) * (x - p.x) + (y - p.y) * (y - p.y);
    }

    friend ostream& operator<<(ostream& os, const Point &p) {
        os << p.x << " " << p.y;
        return os;
    }
};

const Point DIR_TO_POINT[] = {Point(0, -1), Point(1, 0), Point(0, 1), Point(-1, 0)};

// hash Point
namespace std {
    template <>
    struct hash<Point> {
        size_t operator()(const Point& p) const {
            return hash<int>()(p.x) ^ hash<int>()(p.y);
        }
    };
}


class PositionState {
public:
    Point pos;
    DIR dir;

    PositionState() {}
    PositionState(int x, int y, DIR d) : pos(x, y), dir(d) {}

    bool operator==(const PositionState& p) const {
        return pos == p.pos && dir == p.dir;
    }

    friend ostream& operator<<(ostream& os, const PositionState &p) {
        os << p.pos << " " << DIR_TO_CHAR[p.dir];
        return os;
    }
};
namespace std {
    template <>
    struct hash<PositionState> {
        size_t operator()(const PositionState& p) const {
            return hash<Point>()(p.pos) ^ hash<int>()(p.dir);
        }
    };
}

class Robot {
public:
    PositionState backup_pos;
    PositionState pos;
    // vector<vector<short>> visited_points_grid;
    short visited_points_grid[ROWS][COLS];
    bool working = true;

    Robot(int x, int y, DIR d) : pos(x, y, d) {
        backup_pos = pos;
        // visited_points_grid.resize(ROWS, vector<short>(COLS, 0));
        for(int y = 0; y < ROWS; y++) {
            for(int x = 0; x < COLS; x++) {
                visited_points_grid[y][x] = 0;
            }
        }
    }

    void reset() {
        pos = backup_pos;
        working = true;
        for(int y = 0; y < ROWS; y++) {
            for(int x = 0; x < COLS; x++) {
                visited_points_grid[y][x] = 0;
            }
        }
    }

    void move() {
        pos.pos = pos.pos + DIR_TO_POINT[pos.dir];
        // map is toroidal
        pos.pos.x = (pos.pos.x + COLS) % COLS;
        pos.pos.y = (pos.pos.y + ROWS) % ROWS;

        if(visited_points_grid[pos.pos.y][pos.pos.x] & DIR_TO_SHORT_MASK[pos.dir]) {
            working = false;
            return;
        }
        
        visited_points_grid[pos.pos.y][pos.pos.x] |= DIR_TO_SHORT_MASK[pos.dir];
    }

    void update_tile(const char tile) {
        if(tile == VOID) {
            working = false;
        } else if(tile != PLATFORM) {
            pos.dir = CHAR_TO_DIR[tile];
        }
    }

    friend ostream& operator<<(ostream& os, const Robot &r) {
        os << r.pos.pos << " " << DIR_TO_CHAR[r.pos.dir];
        return os;
    }
};

/* #endregion */

/* #region --- MAIN CLASSES ------- */
class State {
    // vector<vector<char>> backup_grid;
    char backup_grid[ROWS][COLS];
public:
    // vector<vector<char>> grid;      // 0 0 in top left     
    char grid[ROWS][COLS];      // 0 0 in top left
    vector<Robot> robots;
    vector<Point> modifiable;

    // ------- constructor -------
    State() {
        // grid.resize(ROWS, vector<char>(COLS, VOID));
        // backup_grid.resize(ROWS, vector<char>(COLS, VOID));
    }
    State(const State &s) {
        // grid = s.grid;
        // backup_grid.resize(ROWS, vector<char>(COLS, VOID));
        for (int y = 0; y < ROWS; y++) {
            for (int x = 0; x < COLS; x++) {
                grid[y][x] = s.grid[y][x];
            }
        }
        robots = s.robots;
        modifiable = s.modifiable;
        backup();
    }

    // ------- update data -------
    void update_grid(const vector<string> &lines) {
        modifiable.clear();

        for (int y = 0; y < ROWS; y++) {
            for (int x = 0; x < COLS; x++) {
                grid[y][x] = lines[y][x];
                backup_grid[y][x] = lines[y][x];

                if(grid[y][x] == PLATFORM) 
                    modifiable.push_back(Point(x, y));
            }
        }
    }

    void update_grid(const Point &p, const char c) {
        grid[p.y][p.x] = c;
        backup_grid[p.y][p.x] = c;
    }

    void update_grid_backup() {
        for(Point &p : modifiable) {
            backup_grid[p.y][p.x] = grid[p.y][p.x];
        }
    }

    // ------- data preservation -------
    void backup() {
        for(Point &p : modifiable)
            backup_grid[p.y][p.x] = grid[p.y][p.x];
    }

    void restore_backup() {
        for(Point &p : modifiable) {
            grid[p.y][p.x] = backup_grid[p.y][p.x];
        }
        for(Robot &r : robots) {
            r.reset();
        }
    }

    void soft_copy(const State &s) {
        for(Point &p : modifiable)
            grid[p.y][p.x] = s.grid[p.y][p.x];
        robots = s.robots;
    }

    // ------- helpers -------
    vector<PositionState> gen_diff(const State &s) {
        vector<PositionState> diff;
        
        for(Point &p : modifiable) {
            if(grid[p.y][p.x] != s.grid[p.y][p.x]) {
                diff.push_back(PositionState(p.x, p.y, CHAR_TO_DIR[s.grid[p.y][p.x]]));
            }
        }

        return diff;
    }

    // ------- simulation -------
    int eval() {    //? evaluates score and calls restore_backup
        const int res = simulate();
        restore_backup();
        return res;
    }

    int simulate() {    //? simulates while overwriting solution
        return simulate_all_robots();
    }

    int simulate_all_robots() {
        int score = 0;
        for(Robot &r : robots) {
            while(true) {
                r.update_tile(grid[r.pos.pos.y][r.pos.pos.x]);
                if(!r.working) break;
                r.move();
                score++;
            }
        }
        return score;
    }

    // ------- print -------
    friend ostream& operator<<(ostream& os, const State &s) {
        for (int y = 0; y < ROWS; y++) {
            for (int x = 0; x < COLS; x++) {
                bool robot_exists = false;

                for (const Robot &r : s.robots) {
                    if(r.pos.pos.y == y && r.pos.pos.x == x) {
                        os << char(tolower(DIR_TO_CHAR[r.pos.dir]));
                        robot_exists = true;
                    }
                }

                if(!robot_exists)
                    os << s.grid[y][x];
            }
            os << '\n';
        }
        return os;
    }
};

class Solution {
public:
    State s;
    // ------- constructor -------
    Solution() : s() {
        parse_input();
    }

    // ------- input -------
    void parse_input() {
        // parse grid
        vector<string> lines;
        for (int i = 0; i < 10; i++) {
            string line;
            getline(cin, line);
            lines.push_back(line);
        }
        s.update_grid(lines);

        // parse robots
        vector<Robot> robots;
        int robot_count;
        cin >> robot_count; cin.ignore();
        for (int i = 0; i < robot_count; i++) {
            int x;
            int y;
            string direction;
            cin >> x >> y >> direction; cin.ignore();

            DIR d;
            switch (direction[0]) {
                case 'U': d = UP; break;
                case 'R': d = RIGHT; break;
                case 'D': d = DOWN; break;
                case 'L': d = LEFT; break;
            }

            robots.push_back(Robot(x, y, d));
        }
        s.robots = robots;
    }

    // ======== SOLVE ========
    // ------- random -------
    int gen_random_dir(State &s, int x, int y) {
        int d = rand() % 4;

        int nx = (x + DIR_TO_POINT[d].x + COLS) % COLS;
        int ny = (y + DIR_TO_POINT[d].y + ROWS) % ROWS;
        
        if(s.grid[ny][nx] != VOID && CHAR_TO_DIR[s.grid[ny][nx]] != (d+2)%4) {
            return d;
        }

        bool tested[4] = {0,0,0,0};
        tested[d] = true;

        for(int i = 0; i < 3; i++) {
            d = rand() % 4;
            while(tested[d]) d = (d+1)%4;
            tested[d] = true;

            nx = (x + DIR_TO_POINT[d].x + COLS) % COLS;
            ny = (y + DIR_TO_POINT[d].y + ROWS) % ROWS;
            
            if(s.grid[ny][nx] != VOID && CHAR_TO_DIR[s.grid[ny][nx]] != (d+2)%4) {
                return d;
            }
        }

        return d;
    }

    // ------- simulated annealing -------
    pair<Point, char> mutate(State &s) {
        const int rand_idx = rand() % s.modifiable.size();
        const Point p = s.modifiable[rand_idx];

        if(s.grid[p.y][p.x] != PLATFORM && random_uniform() < 0.5)
            return {p, PLATFORM};

        const int d = gen_random_dir(s, p.x, p.y);

        return {p, DIR_TO_CHAR[d]};
    }

    vector<PositionState> solve_simulated_annealing(int T = 900) {
        Timer timer;
        double temp_start = 500.;
        double temp_end = 10.;
        double temp = temp_start;
        const int N = 1000;

        int states_analyzed = 0;

        State best(s);
        int best_score = best.eval();
        State current(s);
        int current_score = best_score;

        while(true) {
            auto elapsed = timer.elapsed();
            if(elapsed > T) break;

            const auto time_frac = (double)elapsed / T;
            const auto temp = temp_start * pow((temp_end/temp_start), time_frac); 
            // const auto temp = temp_start + (temp_end - temp_start) * time_frac;

            const int mutations = max(0, (int)(10 * temp / temp_start));

            for(int i = 0; i < N; i++) {
                states_analyzed++;

                auto [p, c] = mutate(current);
                current.grid[p.y][p.x] = c;
                for(int j = 0; j < mutations; j++) {
                    auto temp = mutate(current);
                    current.grid[temp.first.y][temp.first.x] = temp.second;
                }

                int candidate_score = current.simulate();
                int diff = candidate_score - current_score;
                if(diff >= 0 || rand() < exp(diff/temp)) {
                    if(mutations) current.update_grid_backup();
                    else current.update_grid(p, c);

                    current_score = candidate_score;

                    if(candidate_score > best_score) {
                        best = current;
                        best_score = candidate_score;
                    }
                }
                
                current.restore_backup();
            }
        }

        cerr << "States analyzed: " << states_analyzed << '\n';

        return s.gen_diff(best);
    }
};

int main() {
    CHAR_TO_DIR['U']=UP,CHAR_TO_DIR['R']=RIGHT,CHAR_TO_DIR['D']=DOWN,CHAR_TO_DIR['L']=LEFT;
    srand(time(NULL));

    Solution solver;

    vector<PositionState> solution = solver.solve_simulated_annealing(985);
    for(PositionState &p : solution) {
        cout << p.pos.x << " " << p.pos.y << " " << DIR_TO_CHAR[p.dir] << ' ';
    }

    cout << endl;
}
