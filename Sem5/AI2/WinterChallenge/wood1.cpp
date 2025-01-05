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

    Point operator-(const Point& p) const {
        return Point(x - p.x, y - p.y);
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

enum DIR {UP, DOWN, LEFT, RIGHT};
const Point DIRS[] = {Point(0, -1), Point(0, 1), Point(-1, 0), Point(1, 0)};
const char DIR_TO_GEO[] = {'N', 'S', 'W', 'E'};
const unordered_map<char, DIR> GEO_TO_DIR = {{'N', UP}, {'S', DOWN}, {'W', LEFT}, {'E', RIGHT}, {'X', UP}};

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
    DIR direction;
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

    vector<int> my_proteins_cnt = vector<int>(4, 0);

    int active_root_id = -1;

    vector<vector<int>> dist_to_protein = vector<vector<int>>(height, vector<int>(width, 1e8));  //? distance from pos to closest protein
    
    GameState() {}

    void safekeep_harvesters() {
        // for each harvester ensure to not harvest its protein, would be nice: unless there are no more cells to go
        for(auto& [pos, organ] : my_organs) {
            if(organ.type == MY_HARVESTER) {
                Point new_p = pos + DIRS[organ.direction];
                if(proteins.find(new_p) != proteins.end()) {
                    dist_to_protein[new_p.y][new_p.x] = 1e9;
                }
            }
        }
    }

    pair<int, Point> sporers_can_spore() {
        // for each sporer check if can spore
        for(auto& [pos, organ] : my_organs) {
            if(organ.root_id != active_root_id) continue;

            if(organ.type == MY_SPORER) {
                Point new_p = pos + DIRS[organ.direction];
                while(grid[new_p.y][new_p.x] == EMPTY || proteins.find(new_p) != proteins.end()) {
                    if(proteins.find(new_p) != proteins.end()) {
                        return {organ.id, new_p - DIRS[organ.direction] - DIRS[organ.direction]};
                    }
                    new_p = new_p + DIRS[organ.direction];
                }
            }
        }

        return {-1, Point(-1, -1)};
    }

    pair<Point, DIR> plant_sporers() {
        // for each possible sporer check if can reach protein
        for(const auto& [pos, organ] : my_organs) {
            if(organ.root_id != active_root_id) continue;

            for(int i = 0; i < 4; i++) {
                Point new_p = pos + DIRS[i];
                if(grid[new_p.y][new_p.x] != EMPTY && proteins.find(new_p) == proteins.end()) continue;

                for(int j = 0; j < 4; j++) {
                    Point new_new_p = new_p;

                    while(grid[new_new_p.y][new_new_p.x] == EMPTY) {
                        new_new_p = new_new_p + DIRS[j];
                        if(proteins.find(new_new_p) != proteins.end()) {
                            return {new_p, (DIR)j};
                        }
                    }
                }

            }
        }

        return {Point(-1, -1), UP};
    }

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

    pair<Point, DIR> opp_is_neigh() {   // from all my possible new positions find one that is neighbour to opponent
        for(const auto& [pos, organ] : my_organs) {
            if(organ.root_id != active_root_id) continue;

            for(int i = 0; i < 4; i++) {
                for(int j = 0; j < 4; j++) {
                    Point new_p = pos + DIRS[i] + DIRS[j];
                    if(opp_organs.find(new_p) != opp_organs.end()) {
                        return {pos + DIRS[i], (DIR)j};
                    }
                }
            }
        }
        return {Point(-1, -1), UP};
    }

    inline bool is_protein(const Point &p) const {
        return proteins.find(p) != proteins.end();
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


vector<int> my_root_ids;
void read_loop_input(GameState &state) {
    my_root_ids.clear();
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

        if(type == "ROOT" || type == "BASIC" || type == "HARVESTER" || type == "TENTACLE" || type == "SPORER") {
            if(owner == 1) {    // my organ
                state.grid[row][col] = type[0];
                state.my_organs[Point(col, row)] = {Point(col, row), type[0], organ_id, organ_parent_id, organ_root_id, GEO_TO_DIR.find(organ_dir[0])->second};
                if(type == "ROOT") my_root_ids.push_back(organ_id);
            } else {            // opponent's organ
                state.grid[row][col] = tolower(type[0]);
                state.opp_organs[Point(col, row)] = {Point(col, row), type[0], organ_id, organ_parent_id, organ_root_id, GEO_TO_DIR.find(organ_dir[0])->second};
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
    state.my_proteins_cnt = {my_a, my_b, my_c, my_d};

    int opp_a;
    int opp_b;
    int opp_c;
    int opp_d; // opponent's protein stock
    cin >> opp_a >> opp_b >> opp_c >> opp_d; cin.ignore();
    cin >> required_actions_count; cin.ignore();
}

class AI {
public:
    GameState state;

    AI() {}
    
    void recreate_state() {
        state = GameState();
        read_loop_input(state);
        state.calc_dist_to_protein();
        state.safekeep_harvesters();
    }

    int get_parent_id(const Point &pos) {
        // simply find neighbour organ
        for(int dir = 0; dir < 4; dir++) {
            Point new_p = pos + DIRS[dir];
            if(state.my_organs.find(new_p) != state.my_organs.end()) {
                return state.my_organs[new_p].id;
            }
        }
        return 1;   // better not happen
    }

    Point get_closest_to_protein() {
        Point best_pos(-1, -1);
        int best_dist = 1e8;

        for(const auto& [pos, organ] : state.my_organs) {
            if(organ.root_id != state.active_root_id) continue;

            for(int i = 0; i < 4; i++) {
                const Point new_p = organ.pos + DIRS[i];
                // assuming grid surrounded by walls
                // if(new_p.x < 0 || new_p.x >= width || new_p.y < 0 || new_p.y >= height) continue;
                if(state.grid[new_p.y][new_p.x] != EMPTY && !state.is_protein(new_p)) continue;

                // cerr << "Checking " << new_p << " with dist " << state.dist_to_protein[new_p.y][new_p.x] << '\n';

                if(state.dist_to_protein[new_p.y][new_p.x] < best_dist) {
                    best_dist = state.dist_to_protein[new_p.y][new_p.x];
                    best_pos = new_p;
                }
            }
        }

        return best_pos;
    }

    Point greedy_to_protein() {
        Point best = get_closest_to_protein();
        if(best != Point(-1, -1)) return best;

        for(const auto& [pos, organ] : state.my_organs) {
            if(organ.root_id != state.active_root_id) continue;

            for(int i = 0; i < 4; i++) {
                const Point new_p = organ.pos + DIRS[i];
                // assuming grid surrounded by walls
                // if(new_p.x < 0 || new_p.x >= width || new_p.y < 0 || new_p.y >= height) continue;
                if(state.grid[new_p.y][new_p.x] != EMPTY) continue;

                return new_p;
            }
        }

        return Point(-1, -1);
    }
};



int main() {
    cin >> width >> height; cin.ignore();

    AI ai;

    // game loop
    while(1) {
        ai.recreate_state();

        for (int i = 0; i < required_actions_count; i++) {
            ai.state.active_root_id = my_root_ids[i];

            Point move = ai.greedy_to_protein();

            if(move == Point(-1, -1)) {
                cout << "WAIT" << endl;
            } else {
                auto [sporer_id, spore_pos] = ai.state.sporers_can_spore();
                auto [plant_pos, plant_dir] = ai.state.plant_sporers();
                auto [pos_h, dir_h] = ai.state.opp_is_neigh();
                cerr << "Sporess " << sporer_id << ' ' << spore_pos << '\n';
                cerr << "Plants " << plant_pos << ' ' << DIR_TO_GEO[plant_dir] << '\n';
                cerr << "Harvesters " << pos_h << ' ' << DIR_TO_GEO[dir_h] << '\n';
                // try sporers
                // check if can launch spore
                if(sporer_id != -1) {
                    cout << "SPORE " << sporer_id << ' ' << spore_pos << endl;
                } else if(plant_pos != Point(-1, -1) && ai.state.my_proteins_cnt[1] > 0 && ai.state.my_proteins_cnt[3] > 0) {
                    cout << "GROW " << ai.get_parent_id(plant_pos) << ' ' << plant_pos << " SPORER " << DIR_TO_GEO[plant_dir] << endl;
                }

                // stuff

                else if(pos_h != Point(-1, -1) && ai.state.my_proteins_cnt[1] > 0 && ai.state.my_proteins_cnt[2] > 0) {
                    cout << "GROW " << ai.get_parent_id(pos_h) << ' ' << pos_h << " TENTACLE " << DIR_TO_GEO[dir_h] << endl;
                } else if(ai.state.dist_to_protein[move.y][move.x] == 1 && ai.state.my_proteins_cnt[2] > 0 && ai.state.my_proteins_cnt[3] > 0) {
                    DIR dir = UP;
                    for(int i = 0; i < 4; i++) {
                        Point new_p = move + DIRS[i];
                        if(ai.state.proteins.find(new_p) != ai.state.proteins.end()) {
                            dir = (DIR)i;
                            break;
                        }
                    }
                    cout << "GROW " << ai.get_parent_id(move) << ' ' << move << " HARVESTER" << ' ' << DIR_TO_GEO[dir] << endl;
                } else if(ai.state.my_proteins_cnt[0] > 0) {
                    cout << "GROW " << ai.get_parent_id(move) << ' ' << move << " BASIC" << endl;
                    cerr << "Decided to take BASIC " << move << " with dist " << ai.state.dist_to_protein[move.y][move.x] << '\n';
                } else if(ai.state.my_proteins_cnt[2] > 0 && ai.state.my_proteins_cnt[3] > 0) {
                    cout << "GROW " << ai.get_parent_id(move) << ' ' << move << " HARVESTER N" << endl;
                } else if(ai.state.my_proteins_cnt[1] > 0 && ai.state.my_proteins_cnt[2] > 0) {
                    cout << "GROW " << ai.get_parent_id(move) << ' ' << move << " TENTACLE N" << endl;
                } else if(ai.state.my_proteins_cnt[1] > 0 && ai.state.my_proteins_cnt[3] > 0) {
                    cout << "GROW " << ai.get_parent_id(move) << ' ' << move << " SPORER N" << endl;
                } else {
                    cout << "WAIT" << endl;
                }
            }
        }
    }
}