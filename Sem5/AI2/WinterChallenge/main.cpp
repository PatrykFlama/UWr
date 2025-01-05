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
        os << p.x << ' ' << p.y;
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
const DIR DIRS[] = {UP, DOWN, LEFT, RIGHT};
const Point DIR_TO_POINT[] = {Point(0, -1), Point(0, 1), Point(-1, 0), Point(1, 0)};

const char EMPTY = '-', WALL = '#';
const char PA = '1', PB = '2', PC = '3', PD = '4';
const char MY_ROOT = 'R', MY_BASIC = 'B', MY_TENTACLE = 'T', MY_HARVESTER = 'H', MY_SPORER = 'S';
const char OPP_ROOT = 'r', OPP_BASIC = 'b', OPP_TENTACLE = 't', OPP_HARVESTER = 'h', OPP_SPORER = 's';

const char MY_TYPES[] = {MY_ROOT, MY_BASIC, MY_TENTACLE, MY_HARVESTER, MY_SPORER};
string TYPE_NAMES[255];


const int ORGAN_COST[4][4] = {
    {1, 0, 0, 0},
    {0, 1, 1, 0},
    {1000, 1000, 1000, 1000},
    {1000, 1000, 1000, 1000}
};

const int SCORE_PROTEIN_DESTROY = 3;
const int SCORE_PROTEIN_HARVEST = 1;

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
    DIR direction;  // not always needed
};

class Protein {
public:
    Point pos;
    int id;
    char type;
};

class Move {
public:
    int parent_id;
    Point pos;
    char type;
    DIR direction;
};

class GameState {
public:
    vector<string> grid = vector<string>(height, string(width, EMPTY));

    //? ANS: cell wall is created
    int active_owner = 1;   //! what if in real game i and opponent grow onto same cell? is there priority or what

    unordered_map<Point, Organ> my_organs;
    unordered_map<Point, Organ> opp_organs;
    unordered_map<Point, Protein> proteins;

    vector<int> my_proteins_cnt = vector<int>(4, 0);
    vector<int> opp_proteins_cnt = vector<int>(4, 0);

    
    GameState() {}



    // ---- simulation -----
    bool move_is_valid(const Move &move) {
        // assuming grid surrounded by walls
        // if(move.pos.x < 0 || move.pos.x >= width || move.pos.y < 0 || move.pos.y >= height) return false;
        if(grid[move.pos.y][move.pos.x] != EMPTY && proteins.find(move.pos) == proteins.end()) return false;

        if(active_owner) {
            for(int protein = 0; protein < 4; protein++) {
                if(my_proteins_cnt[protein] < ORGAN_COST[move.type - '1'][protein]) return false;
            }
        } else {
            for(int protein = 0; protein < 4; protein++) {
                if(opp_proteins_cnt[protein] < ORGAN_COST[move.type - '1'][protein]) return false;
            }
        }

        return true;
    }

    void _grow_organ(unordered_map<Point, Organ> &organs, vector<int> &proteins_cnt, int parent_id, const Point& pos, const char type, const DIR direction) {
        // pay for organ
        for(int protein = 0; protein < 4; protein++) {
            proteins_cnt[protein] -= ORGAN_COST[type - '1'][protein];
        }

        // destroy protein and gain score (if it exists)
        const char tile_type = grid[pos.y][pos.x];
        if('1' <= tile_type && tile_type <= '4') {
            proteins_cnt[tile_type - '1'] += SCORE_PROTEIN_DESTROY;
            proteins.erase(pos);
        }
        
        // create new organ
        grid[pos.y][pos.x] = type;
        const int my_id = my_organs.size() + opp_organs.size() + 1;

        int root = 1;
        for(DIR d : DIRS) {
            Point new_p = pos + DIR_TO_POINT[d];
            if(organs.find(new_p) != organs.end()) {
                root = organs[new_p].root_id;
                break;
            }
        }

        organs[pos] = {pos, type, my_id, parent_id, root, direction};
    }
    void apply_move(const Move &move) { // assume pos is correct
        if(move.pos != Point(-1, -1)) {
            if(active_owner) {
                _grow_organ(my_organs, my_proteins_cnt, move.parent_id, move.pos, move.type, move.direction);
            } else {
                _grow_organ(opp_organs, opp_proteins_cnt, move.parent_id, move.pos, move.type, move.direction);
            }
        }

        harvest_proteins();
        swap_players();
    }

    void harvest_proteins() {
        // each harvester can harvest 1 protein that it is facing (if it exists)
        for(auto& [pos, organ] : my_organs) {
            if(organ.type == MY_HARVESTER) {
                Point new_p = pos + DIR_TO_POINT[organ.direction];
                if(proteins.find(new_p) != proteins.end()) {
                    my_proteins_cnt[proteins[new_p].type - 'A'] += SCORE_PROTEIN_HARVEST;
                }
            }
        }

        for(auto& [pos, organ] : opp_organs) {
            if(organ.type == OPP_HARVESTER) {
                Point new_p = pos + DIR_TO_POINT[organ.direction];
                if(proteins.find(new_p) != proteins.end()) {
                    opp_proteins_cnt[proteins[new_p].type - 'A'] += SCORE_PROTEIN_HARVEST;
                }
            }
        }
    }

    void swap_players() {
        // swap(my_organs, opp_organs);
        // swap(my_proteins_cnt, opp_proteins_cnt);

        active_owner = 1 - active_owner;
    }

    int eval() {
        return my_organs.size() - opp_organs.size();
    }

    bool is_terminal() {
        bool my_alive = false, opp_alive = false;
        for(int protein = 0; protein < 4; protein++) {
            if(my_proteins_cnt[protein] != 0) my_alive = true;
            if(opp_proteins_cnt[protein] != 0) opp_alive = true;
        }

        return !my_alive || !opp_alive;
    }

    // ------- generate moves -------
    void get_valid_positions(vector<Point> &valid_positions, const unordered_map<Point, Organ> &organs, const vector<int> &proteins_cnt) const {
        // valid positions are empty or protein neighbours
        queue<Point> q;
        for(const auto& [pos, organ] : organs) {
            q.push(pos);
        }

        while(!q.empty()) {
            Point p = q.front();
            q.pop();

            for(DIR d : DIRS) {
                Point new_p = p + DIR_TO_POINT[d];
                if(grid[new_p.y][new_p.x] != EMPTY && proteins.find(new_p) == proteins.end()) continue;
                valid_positions.push_back(new_p);    
            }
        }
    }
    void get_valid_types(vector<char> &valid_types, const unordered_map<Point, Organ> &organs, const vector<int> &proteins_cnt) const {
        // valid types are those that can be paid for
        for(char type : MY_TYPES) {
            bool valid = true;
            for(int protein = 0; protein < 4; protein++) {
                if(proteins_cnt[protein] < ORGAN_COST[type - '1'][protein]) {
                    valid = false;
                    break;
                }
            }

            if(valid) {
                valid_types.push_back(type);
            }
        }
    }

    Move _get_random_valid_move(const unordered_map<Point, Organ> &organs, const vector<int> &proteins_cnt) const {
        vector<Point> valid_positions;
        vector<char> valid_types;   // valid organ types

        get_valid_positions(valid_positions, organs, proteins_cnt);
        get_valid_types(valid_types, organs, proteins_cnt);

        // check if there are any valid moves
        if(valid_positions.empty() || valid_types.empty()) {
            return Move{-1, Point(-1, -1), EMPTY, UP};
        }

        // choose random move
        Point pos = valid_positions[rand() % valid_positions.size()];
        char type = valid_types[rand() % valid_types.size()];

        // if type is harvester choose random direction facing protein,
        // (if not harvester, direction is UP) (if no protein, dir is UP and update type)
        int direction = -1;
        if(type == MY_HARVESTER) {
            for(DIR d : DIRS) {
                Point new_p = pos + DIR_TO_POINT[d];
                if(proteins.find(new_p) != proteins.end()) {
                    direction = d;
                    break;
                }
            }
        }
        if(direction == -1) {
            direction = UP;
            // type = MY_BASIC;     //! TODO RISKY ASSUMPTION THAT WE DONT WANT HARVESTER THAT CANT HARVEST
        }

        // find parent id, which will be any neigbouring organ
        int parent_id = 0;
        for(DIR d : DIRS) {
            Point new_p = pos + DIR_TO_POINT[d];
            if(organs.find(new_p) != organs.end()) {
                parent_id = organs.find(new_p)->second.id;
                break;
            }
        }

        // return move, converting type to adequate player
        return Move{parent_id, pos, (active_owner ? type : (char)tolower(type)), (DIR)direction};
    }
    Move get_random_valid_move() const {  // if no valid move, make empty move <=> pos = Point(-1, -1)
        if(active_owner) {
            return _get_random_valid_move(my_organs, my_proteins_cnt);
        } else {
            return _get_random_valid_move(opp_organs, opp_proteins_cnt);
        }
    }

    vector<Move> _get_all_valid_moves(const unordered_map<Point, Organ> &organs, const vector<int> &proteins_cnt) const {
        vector<Move> valid_moves;
        vector<Point> valid_positions;
        vector<char> valid_types;

        get_valid_positions(valid_positions, organs, proteins_cnt);
        get_valid_types(valid_types, organs, proteins_cnt);

        for(Point pos : valid_positions) {
            for(char type : valid_types) {
                int direction = -1;
                if(type == MY_HARVESTER) {
                    for(DIR d : DIRS) {
                        Point new_p = pos + DIR_TO_POINT[d];
                        if(proteins.find(new_p) != proteins.end()) {
                            direction = d;
                            break;
                        }
                    }
                }
                if(direction == -1) {
                    // continue;   //! TODO RISKY ASSUMPTION THAT WE DONT WANT HARVESTER THAT CANT HARVEST
                    direction = UP;
                }

                int parent_id = 0;
                for(DIR d : DIRS) {
                    Point new_p = pos + DIR_TO_POINT[d];
                    if(organs.find(new_p) != organs.end()) {
                        parent_id = organs.find(new_p)->second.id;
                        break;
                    }
                }

                valid_moves.push_back(Move{parent_id, pos, (active_owner ? type : (char)tolower(type)), (DIR)direction});
            }
        }

        if(valid_moves.empty()) {
            valid_moves.push_back(Move{-1, Point(-1, -1), EMPTY, UP});
        }

        return valid_moves;
    }
    vector<Move> get_all_valid_moves() const {
        if(active_owner) {
            return _get_all_valid_moves(my_organs, my_proteins_cnt);
        } else {
            return _get_all_valid_moves(opp_organs, opp_proteins_cnt);
        }
    }
};


class FlatMonteCarloAgent {
public:
    FlatMonteCarloAgent() {}

    // simulate to the end, return reward
    int simulate(GameState& state) {
        while(!state.is_terminal()) {
            Move move = state.get_random_valid_move();
            state.apply_move(move);
        }
        return state.eval();
    }

    Move choose_best_move(const GameState& state, int time_limit = 90) {
        vector<Move> moves = state.get_all_valid_moves();
        
        vector<double> rewards(moves.size(), 0);
        vector<int> counts(moves.size(), 0);

        timer.reset();
        while(timer.elapsed() < time_limit) {
            GameState state_copy = state;
            int move = rand() % moves.size();
            state_copy.apply_move(moves[move]);
            int reward = simulate(state_copy);
            rewards[move] += reward;
            counts[move]++;
        }


        double best_avg = -1e9;
        int best_move = 0;

        for(int move = 0; move < moves.size(); move++) {
            if(counts[move] > 0) {
                double avg = rewards[move] / counts[move];
                if(avg > best_avg) {
                    best_avg = avg;
                    best_move = move;
                }
            }
        }

        return moves[best_move];
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

        if(type == "ROOT" || type == "BASIC" || type == "TENTACLE" || type == "HARVESTER" || type == "SPORER") {
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
    state.my_proteins_cnt = {my_a, my_b, my_c, my_d};

    int opp_a;
    int opp_b;
    int opp_c;
    int opp_d; // opponent's protein stock
    cin >> opp_a >> opp_b >> opp_c >> opp_d; cin.ignore();
    state.opp_proteins_cnt = {opp_a, opp_b, opp_c, opp_d};

    cin >> required_actions_count; cin.ignore();
}


int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(0);

    srand(time(0));

    TYPE_NAMES[MY_ROOT] = "ROOT";
    TYPE_NAMES[MY_BASIC] = "BASIC";
    TYPE_NAMES[MY_TENTACLE] = "TENTACLE";
    TYPE_NAMES[MY_HARVESTER] = "HARVESTER";
    TYPE_NAMES[MY_SPORER] = "SPORER";
    TYPE_NAMES[OPP_ROOT] = "ROOT";
    TYPE_NAMES[OPP_BASIC] = "BASIC";
    TYPE_NAMES[OPP_TENTACLE] = "TENTACLE";
    TYPE_NAMES[OPP_HARVESTER] = "HARVESTER";
    TYPE_NAMES[OPP_SPORER] = "SPORER";


    cin >> width >> height; cin.ignore();

    GameState state;
    FlatMonteCarloAgent agent;

    // game loop
    while(1) {
        read_loop_input(state);

        for (int i = 0; i < required_actions_count; i++) {
            Move move = agent.choose_best_move(state);

            if(move.pos == Point(-1, -1)) {
                cout << "WAIT" << endl;
            } else {
                cout << "GROW " << move.parent_id << ' ' << move.pos << ' ' << move.type << ' ' << move.direction << endl;
            }
        }
    }
}