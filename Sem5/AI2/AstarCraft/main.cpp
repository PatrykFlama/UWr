#include <bits/stdc++.h>
#include <chrono>
using namespace std;
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

// time
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

/* #endregion */

// ------- CONSTANTS -------
const int ROWS = 10, COLS = 19;
const char VOID = '#', PLATFORM = '.';
enum DIR {UP, RIGHT, DOWN, LEFT};
const char DIR_TO_CHAR[] = {'U', 'R', 'D', 'L'};
unordered_map<char, DIR> CHAR_TO_DIR = {{'U', UP}, {'R', RIGHT}, {'D', DOWN}, {'L', LEFT}};

/* #region --- HELPER CLASSES------- */
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
    PositionState pos;
    bool working = true;
    unordered_map<PositionState, bool> visited;

    Robot(int x, int y, DIR d) : pos(x, y, d) {}

    void move() {
        pos.pos = pos.pos + DIR_TO_POINT[pos.dir];
        // map is toroidal
        pos.pos.x = (pos.pos.x + COLS) % COLS;
        pos.pos.y = (pos.pos.y + ROWS) % ROWS;

        if (visited[pos]) {
            working = false;
        }
        
        visited[pos] = true;
    }

    friend ostream& operator<<(ostream& os, const Robot &r) {
        os << r.pos.pos << " " << DIR_TO_CHAR[r.pos.dir];
        return os;
    }
};

/* #endregion */

/* #region --- MAIN CLASSES ------- */
class State {
public:
    vector<vector<char>> grid;      // 0 0 in top left
    vector<Robot> robots;

    // ------- constructor -------
    State() {
        grid.resize(ROWS, vector<char>(COLS, VOID));
    }
    State(const State &s) {
        grid = s.grid;
        robots = s.robots;
    }

    // ------- update data -------
    void update_grid(vector<string> &lines) {
        for (int y = 0; y < ROWS; y++) {
            for (int x = 0; x < COLS; x++) {
                grid[y][x] = lines[y][x];
            }
        }
    }

    void update_robots(vector<Robot> &r) {
        robots = r;
    }

    // ------- simulation -------
    int eval() {
        State temp(*this);
        return simulate(temp);
    }

    int simulate_single_step(State &s) {
        int score = 0;
        for (Robot &r : s.robots) {
            if(!r.working) continue;   // robot does not work anymore
            score++;

            char robot_tile = grid[r.pos.pos.y][r.pos.pos.x];
            if(robot_tile != PLATFORM && robot_tile != VOID) {
                r.pos.dir = CHAR_TO_DIR[robot_tile];
            }
            
            r.move();

            robot_tile = grid[r.pos.pos.y][r.pos.pos.x];
            if(robot_tile == VOID) {
                r.working = false;
            }
        }

        return score;
    }

    int simulate(State &s) {
        int score, total = 0;
        while((score = simulate_single_step(s)) != 0) { // while some robots are still working
            total += score;
        }
        return total;
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
        parse_input(s);
    }

    // ------- input -------
    void parse_input(State &s) {
        vector<string> lines;
        for (int i = 0; i < 10; i++) {
            string line;
            getline(cin, line);
            lines.push_back(line);
        }

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

        s.update_grid(lines);
        s.update_robots(robots);
    }

    // ------- solve -------
    vector<PositionState> solve_random(int T = 300, float place_prob = 0.5) {
        Timer timer;
        vector<PositionState> best_solution;
        int best_score = 0;

        while(timer.elapsed() < T) {
            State temp(s);
            vector<PositionState> solution;

            for(int y = 0; y < temp.grid.size(); y++) {
                for(int x = 0; x < temp.grid[y].size(); x++) {
                    if(temp.grid[y][x] != PLATFORM) continue;
                    if(rand() % 1000 > place_prob * 1000) continue;

                    const int d = rand() % 4;
                    temp.grid[y][x] = DIR_TO_CHAR[d];
                    solution.push_back(PositionState(x, y, (DIR)d));
                }
            }

            int temp_score = temp.eval();
            if(temp_score > best_score) {
                best_solution = solution;
                best_score = temp_score;
            }
        }

        return best_solution;
    }
};

int main() {
    srand(time(NULL));

    Solution solver;

    vector<PositionState> solution = solver.solve_random(900, rand() % 1000 / 1000.0);
    for(PositionState &p : solution) {
        cout << p.pos.x << " " << p.pos.y << " " << DIR_TO_CHAR[p.dir] << ' ';
    }

    cout << endl;
}
