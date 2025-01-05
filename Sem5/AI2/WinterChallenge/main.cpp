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


const Point DIRS[] = {Point(0, -1), Point(0, 1), Point(-1, 0), Point(1, 0)};

const char EMPTY = '-', WALL = '#';
const char PA = '1', PB = '2', PC = '3', PD = '4';
const char MY_ROOT = 'R', MY_BASIC = 'B', MY_TENTACLE = 'T', MY_HARVESTER = 'H', MY_SPORER = 'S';
const char OPP_ROOT = 'r', OPP_BASIC = 'b', OPP_TENTACLE = 't', OPP_HARVESTER = 'h', OPP_SPORER = 's';


int width, height;
int required_actions_count;
Timer timer;


class Organ {
public:
    Point pos;
    char type;
    int id;
    int parent_id;
    int root_id;
};

class Protein {
public:
    Point pos;
    int id;
    char type;
};

class GameState {
public:
    vector<string> grid = vector<string>(height, string(width, EMPTY));

    unordered_map<Point, Organ> my_organs;
    unordered_map<Point, Organ> opp_organs;
    unordered_map<Point, Protein> proteins;

    vector<vector<int>> dist_to_protein = vector<vector<int>>(height, vector<int>(width, 1e9));  //? distance from pos to closest protein
    
    GameState() {}


    void calc_dist_to_protein() {
        priority_queue<pair<int, Point>> q;
        for(const auto& [pos, protein] : proteins) {
            q.push({0, protein.pos});
            dist_to_protein[protein.pos.y][protein.pos.x] = 0;
        }

        while(!q.empty()) {
            auto [dist, p] = q.top();
            q.pop();

            for(int i = 0; i < 4; i++) {
                Point new_p = p + DIRS[i];
                // assuming grid surrounded by walls
                // if(new_p.x < 0 || new_p.x >= width || new_p.y < 0 || new_p.y >= height) continue;
                if(grid[new_p.y][new_p.x] == WALL) continue;

                if(dist_to_protein[new_p.y][new_p.x] > dist_to_protein[p.y][p.x] + 1) {
                    dist_to_protein[new_p.y][new_p.x] = dist_to_protein[p.y][p.x] + 1;
                    q.push({-dist_to_protein[new_p.y][new_p.x], new_p});
                }
            }
        }
    }

    inline bool is_protein(const Point &p) const {
        return proteins.find(p) != proteins.end();
    }

    int get_parent_id(const Point &pos) {
        // simply find neighbour organ
        for(int dir = 0; dir < 4; dir++) {
            Point new_p = pos + DIRS[dir];
            if(my_organs.find(new_p) != my_organs.end()) {
                return my_organs[new_p].id;
            }
        }
    }

    Point get_closest_to_protein() {
        Point best_pos(-1, -1);
        int best_dist = 1e9;

        for(const auto& [pos, organ] : my_organs) {
            for(int i = 0; i < 4; i++) {
                const Point new_p = organ.pos + DIRS[i];
                // assuming grid surrounded by walls
                // if(new_p.x < 0 || new_p.x >= width || new_p.y < 0 || new_p.y >= height) continue;
                if(grid[new_p.y][new_p.x] != EMPTY && !is_protein(new_p)) continue;

                cerr << "Checking " << new_p << " with dist " << dist_to_protein[new_p.y][new_p.x] << '\n';

                if(dist_to_protein[new_p.y][new_p.x] < best_dist) {
                    best_dist = dist_to_protein[new_p.y][new_p.x];
                    best_pos = new_p;
                }
            }
        }

        return best_pos;
    }

    Point greedy_to_protein() {
        Point best = get_closest_to_protein();
        if(best != Point(-1, -1)) return best;

        for(const auto& [pos, organ] : my_organs) {
            for(int i = 0; i < 4; i++) {
                const Point new_p = organ.pos + DIRS[i];
                // assuming grid surrounded by walls
                // if(new_p.x < 0 || new_p.x >= width || new_p.y < 0 || new_p.y >= height) continue;
                if(grid[new_p.y][new_p.x] != EMPTY) continue;

                return new_p;
            }
        }

        return Point(-1, -1);
    }

    void debug_print_grid() {
        for(int i = 0; i < height; i++) {
            for(int j = 0; j < width; j++) {
                if(grid[i][j] == EMPTY) cerr << dist_to_protein[i][j] << ' ';
                else if(is_protein(Point(j, i))) cerr << proteins[Point(j, i)].type << ' ';
                else cerr << grid[i][j] << ' ';
            }
            cerr << '\n';
        }
    }
};


void read_loop_input(GameState &state) {
    state.proteins.clear();

    int entity_count;
    cin >> entity_count; cin.ignore();
    for (int i = 0; i < entity_count; i++) {
        int col;
        int row; // grid coordinate

        string type; // WALL, ROOT, BASIC, TENTACLE, HARVESTER, SPORER, A, B, C, D
        int owner; // 1 if your organ, 0 if enemy organ, -1 if neither
        int organ_id; // id of this entity if it's an organ, 0 otherwise
        string organ_dir; // N,E,S,W or X if not an organ
        int organ_parent_id;
        int organ_root_id;
        cin >> col >> row >> type >> owner >> organ_id >> organ_dir >> organ_parent_id >> organ_root_id; cin.ignore();

        if(type == "ROOT" || type == "BASIC") {
            if(owner == 1) {    // my organ
                state.grid[row][col] = type[0];
                state.my_organs[Point(col, row)] = {Point(col, row), type[0], organ_id, organ_parent_id, organ_root_id};
            } else {            // opponent's organ
                state.grid[row][col] = tolower(type[0]);
                state.opp_organs[Point(col, row)] = {Point(col, row), type[0], organ_id, organ_parent_id, organ_root_id};
            }
        } else if(type == "WALL") {
            state.grid[row][col] = WALL;
        } else {    // protein
            state.grid[row][col] = type[0] - 'A' + '1';
            state.proteins[Point(col, row)] = {Point(col, row), organ_id, type[0]};
        }
    }

    int my_a;
    int my_b;
    int my_c;
    int my_d; // your protein stock
    cin >> my_a >> my_b >> my_c >> my_d; cin.ignore();

    int opp_a;
    int opp_b;
    int opp_c;
    int opp_d; // opponent's protein stock
    cin >> opp_a >> opp_b >> opp_c >> opp_d; cin.ignore();
    cin >> required_actions_count; cin.ignore();
}


int main() {
    cin >> width >> height; cin.ignore();


    // game loop
    while(1) {
        GameState state;
        read_loop_input(state);
        state.calc_dist_to_protein();

        state.debug_print_grid();

        for (int i = 0; i < required_actions_count; i++) {
            Point move = state.greedy_to_protein();

            if(move == Point(-1, -1)) {
                cout << "WAIT" << endl;
            } else {
                cout << "GROW " << state.get_parent_id(move) << ' ' << move << " BASIC" << endl;
                cerr << "Decided to take " << move << " with dist " << state.dist_to_protein[move.y][move.x] << '\n';
            }
        }
    }
}