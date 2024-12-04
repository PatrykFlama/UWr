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

#define DEBUG 0
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
        return x < p.x || (x == p.x && y < p.y);
    }

    int int_dist(const Point& p) const {
        return (x - p.x) * (x - p.x) + (y - p.y) * (y - p.y);
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


const int TURNS = 200;
const int MISSED_PRESENTS = 30;
const int GARGOYLE_SPEED = 150;
const int WIDTH = 1920;
const int HEIGHT = 750;

int gargoyles_per_player;
Timer timer;
string message = "";


class Gargoyle {
public:
    Point pos;
    int cooldown;

    Gargoyle() : pos(0, 0), cooldown(0) {}
    Gargoyle(Point pos, int cooldown) : pos(pos), cooldown(cooldown) {}
};

class Present {
public:
    int id;
    Point pos;
    int value;
    int vy;

    Present() : id(0), pos(0, 0), value(0), vy(0) {}
    Present(int id, Point pos, int value, int vy) : id(id), pos(pos), value(value), vy(vy) {}
};

class State {
public:
    list<Present> presents;

    vector<Gargoyle> my_gargoyle;
    vector<Gargoyle> opp_gargoyle;
    int my_score;
    int opp_score;

    vector<Point> my_locked_destination;
    vector<Point> opp_locked_destination;

    //? instead of SM-MCTS: in first 'half-turn' i play
    //? then in second half-turn opponent makes his move
    //? and since we completed entire turn, scores are calculated
    bool is_my_turn;

    int turns_left;
    int missed_presents_to_end;

    State() : is_my_turn(true), my_score(0), opp_score(0), turns_left(TURNS), missed_presents_to_end(MISSED_PRESENTS) {}

    bool isTerminal() const {
        return turns_left == 0 || missed_presents_to_end == 0;
    }

    int eval() const {
        return my_score - opp_score;
    }
    int main_player_eval() const {
        return (is_my_turn ? 1 : -1) * (my_score - opp_score);
    }
    int main_player_score() const {
        return is_my_turn ? my_score : opp_score;
    }

    vector<Point> get_main_player_pos() const {
        vector<Point> res;
        if(is_my_turn) {
            for(const Gargoyle &g : my_gargoyle) {
                res.push_back(g.pos);
            }
        } else {
            for(const Gargoyle &g : opp_gargoyle) {
                res.push_back(g.pos);
            }
        }
        return res;
    }

    void swap_roles() {
        swap(my_gargoyle, opp_gargoyle);
        swap(my_score, opp_score);
        swap(my_locked_destination, opp_locked_destination);
        is_my_turn = !is_my_turn;
    }

    Point normalizeDestination(Point p, const Point &gargoyle) const {
        if(gargoyle.int_dist(p) <= GARGOYLE_SPEED*GARGOYLE_SPEED) return p;

        const double dist = gargoyle.dist(p);
        const double dx = p.x - gargoyle.x;
        const double dy = p.y - gargoyle.y;

        p.x = gargoyle.x + (int)(GARGOYLE_SPEED * dx / dist);
        p.y = gargoyle.y + (int)(GARGOYLE_SPEED * dy / dist);   

        return p;
    }

    void applyDestination(const vector<Point> &my_destination) {
        for(int i = 0; i < my_gargoyle.size(); i++) {
            my_gargoyle[i].pos = normalizeDestination(my_destination[i], my_gargoyle[i].pos);
            my_locked_destination[i] = my_destination[i];
        }

        if(is_my_turn) {
            swap_roles();
            return;
        }

        list<Present>::iterator present = presents.begin();
        while(present != presents.end()) {
            // first presents fall
            present->pos.y -= present->vy;
            if(present->pos.y < 0) {
                missed_presents_to_end--;
                present = presents.erase(present++);
                continue;
            }

            // then it is the end of the turn and we calculate score
            int my_points = 0;
            int opp_points = 0;
            for(const Gargoyle &g : my_gargoyle) {
                if(g.pos.int_dist(present->pos) <= 30*30) {
                    my_points = present->value;
                }
            }

            for(const Gargoyle &g : opp_gargoyle) {
                if(g.pos.int_dist(present->pos) <= 30*30) {
                    opp_points = present->value;
                }
            }

            if(my_points || opp_points) {
                my_score += my_points;
                opp_score += opp_points;
                present = presents.erase(present++);
            } else {
                present->pos.y += present->vy;
                if(present->pos.y > HEIGHT) {
                    missed_presents_to_end--;
                    present = presents.erase(present++);
                } else {
                    ++present;
                }
            }
        }

        --turns_left;
        swap_roles();
    }

    vector<Point> legalDestinations_presentsPredict_single(int gargoyle) const {
        if(my_locked_destination[gargoyle] != Point(0, 0)) 
            return {my_locked_destination[gargoyle]};

        const Point &gargoyle_pos = my_gargoyle[gargoyle].pos;
        vector<Point> actions;

        for(const Present &p : presents) {
            // find the first round when we can catch the present
            Point new_pos = p.pos;
            for(int round = 1; round <= 20; round++) {       // kinda slow, but who cares
                new_pos.y -= p.vy;  // where the present will be at the end of the round
                if(new_pos.y < 0) {
                    break;    // present fell on the ground
                }

                const int rounds = (int)ceil(my_gargoyle[gargoyle].pos.dist(new_pos) / (double)GARGOYLE_SPEED);
                if(rounds <= round) {
                    actions.push_back(new_pos);
                    break;
                }
            }
        }

        if(actions.size() == 0) return {Point(WIDTH/2, HEIGHT/2)};

        return actions;
    }

    vector<vector<Point>> legalDestinations_presentsPredict() const {
        vector<vector<Point>> res;  // vector of vectors of size my_gargoyle.size() - every possible combination of destinations

        // assuming there are 3 gargoyles
        vector<Point> actions0 = legalDestinations_presentsPredict_single(0);
        vector<Point> actions1 = legalDestinations_presentsPredict_single(1);
        vector<Point> actions2 = legalDestinations_presentsPredict_single(2);

        for(const Point &p0 : actions0) {
            for(const Point &p1 : actions1) {
                for(const Point &p2 : actions2) {
                    if(p0 == p1 || p0 == p2 || p1 == p2) continue;
                    res.push_back({p0, p1, p2});
                }
            }
        }

        return res;
    }

    vector<Point> getRandomDestination_presentsPredict() const {
        // 3 random presents
        vector<Point> res;

        for(int i = 0; i < 3; i++) {
            int ptr = rand() % presents.size();
            for(const Present &p : presents) {
                if(ptr-- == 0) {
                    for(int round = 1; round < 20; round++) {
                        Point new_pos = {p.pos.x, p.pos.y - p.vy * round};
                        if(new_pos.y < 0) break;

                        const int rounds = (int)ceil(my_gargoyle[i].pos.dist(new_pos) / GARGOYLE_SPEED);
                        if(rounds == round) {
                            res.push_back(new_pos);
                            break;
                        }
                    }
                }
            }
        }

        return res;
    }

    // friend ostream& operator<<(ostream& os, const State &s) {
    //     os << "My score: " << s.my_score << '\n';
    //     os << "Opp score: " << s.opp_score << '\n';
    //     os << "My gargoyle: " << s.my_gargoyle.pos << " " << s.my_gargoyle.cooldown << '\n';
    //     os << "Opp gargoyle: " << s.opp_gargoyle.pos << " " << s.opp_gargoyle.cooldown << '\n';
    //     os << "My turn: " << (s.is_my_turn ? "true" : "false") << '\n';
    //     os << "Missed presents to end: " << s.missed_presents_to_end << '\n';
    //     os << "Turns left: " << s.turns_left << '\n';
    //     os << "Presents (" << s.presents.size() << "): " << '\n';
    //     for(const Present &p : s.presents) {
    //         os << p.id << " " << p.pos << " " << p.value << " " << p.vy << '\n';
    //     }
    //     return os;
    // }

    // void short_debug() {
    //     if(is_my_turn) {
    //         cerr << my_gargoyle.pos << " / " << opp_gargoyle.pos << ' ';
    //         cerr << my_score << " / " << opp_score << '\n';
    //     } else {
    //         cerr << opp_gargoyle.pos << " / " << my_gargoyle.pos << ' ';
    //         cerr << opp_score << " / " << my_score << '\n';
    //     }
    // }
};


class Node {
public:
    State state;
    Node *parent;
    vector<Node*> children;
    vector<vector<Point>> to_vis;
    int visits = 0;
    double reward = 0.0;

    Node(State s, Node *p = nullptr) : state(s), parent(p) {
        to_vis = state.legalDestinations_presentsPredict();
    }

    bool isFullyExpanded() const {
        return to_vis.empty();
    }

    vector<Point> getUnvisitedChild() {
        vector<Point> res = to_vis.back();
        to_vis.pop_back();
        return res;
    }

    bool isTerminal() {
        return state.isTerminal();
    }

    Node *bestChild(double explorationWeight = 1.0) const {
        Node *best = nullptr;
        double bestValue = -numeric_limits<double>::infinity();

        for (Node *child : children) {
            // TODO probably should be positive and not so big
            double uctValue = (double)child->reward / (double)(child->visits + 1e-6) +
                            explorationWeight * sqrt(2*log(visits + 1) / (double)(child->visits + 1e-6));
            if(uctValue > bestValue) {
                bestValue = uctValue;
                best = child;
            }
        }

        return best;
    }
};


class Minimax {
public:
    Minimax() {}

    int minimax(State &state, int depth, bool isMax) {
        if(depth == 0 || state.isTerminal()) {
            return state.eval();
        }

        if(isMax) {
            int best = -numeric_limits<int>::infinity();
            for(const vector<Point> &action : state.legalDestinations_presentsPredict()) {
                State newState = state;
                newState.applyDestination(action);
                best = max(best, minimax(newState, depth-1, !isMax));
            }
            return best;
        } else {
            int best = numeric_limits<int>::infinity();
            for(const vector<Point> &action : state.legalDestinations_presentsPredict()) {
                State newState = state;
                newState.applyDestination(action);
                best = min(best, minimax(newState, depth-1, !isMax));
            }
            return best;
        }
    }

    vector<Point> getMiniMax(State state, int depth) {
        int best = INT_MIN;
        vector<Point> bestAction = state.get_main_player_pos();

        for(const vector<Point> &action : state.legalDestinations_presentsPredict()) {
            State newState = state;
            newState.applyDestination(action);
            int val = minimax(newState, depth, false);
            if(val > best) {
                best = val;
                bestAction = action;
            }
        }

        return bestAction;
    }


    int alphaBeta(State &state, int depth, int alpha, int beta, bool isMax) {
        if(depth == 0 || state.isTerminal()) {
            return state.eval();
        }

        if(isMax) {
            int best = INT_MIN;
            for(const vector<Point> &action : state.legalDestinations_presentsPredict()) {
                State newState = state;
                newState.applyDestination(action);
                best = max(best, alphaBeta(newState, depth-1, alpha, beta, !isMax));
                alpha = max(alpha, best);
                if(beta <= alpha) break;
            }
            return best;
        } else {
            int best = INT_MAX;
            for(const vector<Point> &action : state.legalDestinations_presentsPredict()) {
                State newState = state;
                newState.applyDestination(action);
                best = min(best, alphaBeta(newState, depth-1, alpha, beta, !isMax));
                beta = min(beta, best);
                if(beta <= alpha) break;
            }
            return best;
        }
    }

    vector<Point> getAlphaBeta(State state, int depth) {
        int best = INT_MIN;
        vector<Point> bestAction = state.get_main_player_pos();

        for(const vector<Point> &action : state.legalDestinations_presentsPredict()) {
            State newState = state;
            newState.applyDestination(action);
            int val = alphaBeta(newState, depth, INT_MIN, INT_MAX, false);
            if(val > best) {
                best = val;
                bestAction = action;
            }
        }

        return bestAction;
    }
};


class AI {
    Minimax minimax;
public:
    AI() {}

    inline vector<Point> getFirstAction(State &state) {
        return minimax.getAlphaBeta(state, 3201);
    }

    inline vector<Point> getAction(State &state) {
        return minimax.getAlphaBeta(state, 191);
    }
};

void read_loop_input(State &s) {
    cin >> s.missed_presents_to_end; cin.ignore();
    
    cin >> s.my_score; cin.ignore();
    s.my_gargoyle.clear();
    for (int i = 0; i < gargoyles_per_player; i++) {
        int x, y, cooldown;
        cin >> x >> y >> cooldown; cin.ignore();
        s.my_gargoyle.push_back(Gargoyle(Point(x, y), cooldown));
    }

    cin >> s.opp_score; cin.ignore();
    s.opp_gargoyle.clear();
    for (int i = 0; i < gargoyles_per_player; i++) {
        int x, y, cooldown;
        cin >> x >> y >> cooldown; cin.ignore();
        s.opp_gargoyle.push_back(Gargoyle(Point(x, y), cooldown));
    }

    int presents_count;
    cin >> presents_count; cin.ignore();

    s.presents.clear();
    for (int i = 0; i < presents_count; i++) {
        int id, x, y, value, vy;
        cin >> id >> value >> x >> y >> vy; cin.ignore();
        s.presents.push_back(Present(id, Point(x, y), value, vy));
    }
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    srand(time(0));
    
    State curr_state;

    cin >> gargoyles_per_player; cin.ignore();
    read_loop_input(curr_state);

    AI ai;
    timer.reset();

    vector<Point> res = ai.getFirstAction(curr_state);

    for(auto &action : res) {
        cout << "FLY " << action << endl;
    }

    while(1) {
        read_loop_input(curr_state);


        timer.reset();
        vector<Point> res = ai.getAction(curr_state);

        for(auto &action : res) {
            cout << "FLY " << action << ' ' << message << endl;
        }
    }
}