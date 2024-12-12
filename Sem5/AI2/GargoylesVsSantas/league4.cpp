// #pragma GCC optimize("Ofast,inline,tracer")
// #pragma GCC optimize("unroll-loops,vpt,split-loops,unswitch-loops,omit-frame-pointer,inline")
// #pragma GCC option("march=native","tune=native","no-zero-upper")            //Enable AVX
// #pragma GCC target("arch=haswell,tune=haswell")
// #pragma GCC target("aes,abm,align-stringops,avx,avx2,bmi,bmi2,crc32,cx16,f16c,fma,fsgsbase,fxsr,hle,ieee-fp,lzcnt,mmx,movbe,mwait,pclmul,popcnt,rdrnd,sahf,sse,sse2,sse3,sse4,sse4.1,sse4.2,ssse3,xsave,xsaveopt")
// #include <x86intrin.h>                                                      //AVX/SSE Extensions


/*
League 1 - minimax with aggro with timeout protection
League 2 - minimax with aggro
League 3 - minimax with aggro
League 4
*/

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

vector<Gargoyle> all_my_gargoyles;
vector<Gargoyle> all_opp_gargoyles;


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

    Gargoyle my_gargoyle;
    Gargoyle opp_gargoyle;
    int my_score;
    int opp_score;

    pair<Point, Point> my_locked_destination;
    pair<Point, Point> opp_locked_destination;

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

    Point get_main_player_pos() const {
        return is_my_turn ? my_gargoyle.pos : opp_gargoyle.pos;
    }

    void swap_roles() {
        swap(my_gargoyle, opp_gargoyle);
        swap(my_score, opp_score);
        swap(my_locked_destination, opp_locked_destination);
        is_my_turn = !is_my_turn;
    }

    Point normalizeDestination(Point p) const {
        if(my_gargoyle.pos.int_dist(p) <= GARGOYLE_SPEED*GARGOYLE_SPEED) return p;

        const double dist = my_gargoyle.pos.dist(p);
        const double dx = p.x - my_gargoyle.pos.x;
        const double dy = p.y - my_gargoyle.pos.y;

        p.x = my_gargoyle.pos.x + (int)(GARGOYLE_SPEED * dx / dist);
        p.y = my_gargoyle.pos.y + (int)(GARGOYLE_SPEED * dy / dist);   

        return p;
    }

    void applyDestination(const Point &my_destination) {
        my_gargoyle.pos = normalizeDestination(my_destination);
        my_locked_destination.first = my_destination;

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
            const int my_points = (my_gargoyle.pos.int_dist(present->pos) <= 30*30) ? present->value : 0;
            const int opp_points = (opp_gargoyle.pos.int_dist(present->pos) <= 30*30) ? present->value : 0;

            if(my_points) {
                my_locked_destination.first = Point(0, 0);
                if(present->pos == opp_locked_destination.first) {
                    opp_locked_destination.first = Point(0, 0);
                }
            }
            if(opp_points) {
                opp_locked_destination.first = Point(0, 0);
                if(present->pos == my_locked_destination.first) {
                    my_locked_destination.first = Point(0, 0);
                }
            }

            if(my_points || opp_points) {
                my_score += my_points;
                opp_score += opp_points;
                present = presents.erase(present++);
            } else {
                ++present;
            }
        }

        --turns_left;
        swap_roles();
    }

    vector<Point> legalActions_all() const {
        const Point &gargoyle_pos = my_gargoyle.pos;
        vector<Point> actions;

        const int step = GARGOYLE_SPEED / 10;
        for(int nx = gargoyle_pos.x - GARGOYLE_SPEED; nx <= gargoyle_pos.x + GARGOYLE_SPEED; nx += step) {
            for(int ny = gargoyle_pos.y - GARGOYLE_SPEED; ny <= gargoyle_pos.y + GARGOYLE_SPEED; ny += step) {
                if(gargoyle_pos.int_dist({nx, ny}) > GARGOYLE_SPEED*GARGOYLE_SPEED || 
                   nx < 0 || nx >= WIDTH || ny < 0 || ny >= HEIGHT || 
                   (nx == gargoyle_pos.x && ny == gargoyle_pos.y)) continue;
                actions.push_back({nx, ny});
            }
        }

        return actions;
    }

    vector<pair<Point, Point>> legalDestinations_presentsPredict() const {
        if(my_locked_destination.first != Point(0, 0)) 
            return {my_locked_destination};

        const Point &gargoyle_pos = my_gargoyle.pos;
        vector<pair<Point, Point>> actions;

        for(const Present &p : presents) {
            // find the first round when we can catch the present
            Point new_pos = p.pos;
            for(int round = 1; round <= 20; round++) {       // kinda slow, but who cares
                new_pos.y -= p.vy;  // where the present will be at the end of the round
                if(new_pos.y < 0) {
                    break;    // present fell on the ground
                }

                const int rounds = (int)ceil(my_gargoyle.pos.dist(new_pos) / (double)GARGOYLE_SPEED);
                if(rounds <= round) {
                    actions.push_back({new_pos, p.pos});
                    break;
                }
            }
        }

        if(actions.size() == 0) return {{Point(WIDTH/2, HEIGHT/2), Point(WIDTH/2, HEIGHT/2)}};

        return actions;
    }

    Point getRandomDestination_presentsPredict() const {
        const Point &gargoyle_pos = my_gargoyle.pos;
        int ptr = rand() % presents.size();
        for(const Present &p : presents) {
            if(ptr-- == 0) {
                for(int round = 1; round < 20; round++) {
                    Point new_pos = {p.pos.x, p.pos.y - p.vy * round};
                    if(new_pos.y < 0) break;

                    const int rounds = (int)ceil(gargoyle_pos.dist(new_pos) / GARGOYLE_SPEED);
                    if(rounds == round) {
                        return new_pos;
                    }
                }
            }
        }

        return {WIDTH/2, HEIGHT/2};
    }

    friend ostream& operator<<(ostream& os, const State &s) {
        os << "My score: " << s.my_score << '\n';
        os << "Opp score: " << s.opp_score << '\n';
        os << "My gargoyle: " << s.my_gargoyle.pos << " " << s.my_gargoyle.cooldown << '\n';
        os << "Opp gargoyle: " << s.opp_gargoyle.pos << " " << s.opp_gargoyle.cooldown << '\n';
        os << "My turn: " << (s.is_my_turn ? "true" : "false") << '\n';
        os << "Missed presents to end: " << s.missed_presents_to_end << '\n';
        os << "Turns left: " << s.turns_left << '\n';
        os << "Presents (" << s.presents.size() << "): " << '\n';
        for(const Present &p : s.presents) {
            os << p.id << " " << p.pos << " " << p.value << " " << p.vy << '\n';
        }
        return os;
    }

    void short_debug() {
        if(is_my_turn) {
            cerr << my_gargoyle.pos << " / " << opp_gargoyle.pos << ' ';
            cerr << my_score << " / " << opp_score << '\n';
        } else {
            cerr << opp_gargoyle.pos << " / " << my_gargoyle.pos << ' ';
            cerr << opp_score << " / " << my_score << '\n';
        }
    }
};



class Minimax {
public:
    Minimax() {}

    int alphaBeta(State &state, int depth, int alpha, int beta, bool isMax) {
        if(depth == 0 || state.isTerminal() || state.presents.size() == 0) {
            return state.eval();
        }

        const auto &actions = state.legalDestinations_presentsPredict();
        if(isMax) {
            int best = INT_MIN;
            if(actions.size() > 1) {
                for(const auto &[action, pres] : actions) {
                    State newState = state;
                    newState.applyDestination(action);
                    best = max(best, alphaBeta(newState, depth-1, alpha, beta, !isMax));
                    alpha = max(alpha, best);
                    if(beta <= alpha) break;
                }
            } else {
                for(const auto &[action, pres] : actions) {
                    state.applyDestination(action);
                    best = max(best, alphaBeta(state, depth-1, alpha, beta, !isMax));
                    alpha = max(alpha, best);
                    if(beta <= alpha) break;
                }
            }
            return best;
        } else {
            int best = INT_MAX;
            if(actions.size() > 1) {
                for(const auto &[action, pres] : actions) {
                    State newState = state;
                    newState.applyDestination(action);
                    best = min(best, alphaBeta(newState, depth-1, alpha, beta, !isMax));
                    beta = min(beta, best);
                    if(beta <= alpha) break;
                }
            } else {
                for(const auto &[action, pres] : actions) {
                    state.applyDestination(action);
                    best = min(best, alphaBeta(state, depth-1, alpha, beta, !isMax));
                    beta = min(beta, best);
                    if(beta <= alpha) break;
                }
            }
            return best;
        }
    }

    pair<Point, Point> getAlphaBeta(State state, int depth) {
        int best = INT_MIN;
        Point bestAction = state.get_main_player_pos();
        Point bestPres = state.get_main_player_pos();

        for(const auto &[action, pres] : state.legalDestinations_presentsPredict()) {
            State newState = state;
            newState.applyDestination(action);
            int val = alphaBeta(newState, depth, INT_MIN, INT_MAX, false);
            if(val > best) {
                best = val;
                bestAction = action;
                bestPres = pres;
            }
        }

        return {bestAction, bestPres};
    }
};


class AI {
    Minimax minimax;
public:
    AI() {}

    inline vector<Point> getFirstAction(State &state) {
        vector<Point> res(3);
        for(int i = 0; i < 3; i++) {
            state.my_gargoyle = all_my_gargoyles[i];
            state.opp_gargoyle = all_opp_gargoyles[i];
            
            auto t_res = minimax.getAlphaBeta(state, 3201);
            res[i] = t_res.first;

            // erase the present that we are going to catch
            for(auto it = state.presents.begin(); it != state.presents.end(); ++it) {
                if(it->pos == t_res.second) {
                    state.presents.erase(it);
                    break;
                }
            }
        }
        return res;
    }

    inline vector<Point> getAction(State &state) {
        vector<Point> res(3);
        for(int i = 0; i < 3; i++) {
            state.my_gargoyle = all_my_gargoyles[i];
            state.opp_gargoyle = all_opp_gargoyles[i];
            
            auto t_res = minimax.getAlphaBeta(state, 51);
            res[i] = t_res.first;

            // erase the present that we are going to catch
            for(auto it = state.presents.begin(); it != state.presents.end(); ++it) {
                if(it->pos == t_res.second) {
                    state.presents.erase(it);
                    break;
                }
            }
        }
        return res;
    }
};

void read_loop_input(State &s) {
    cin >> s.missed_presents_to_end; cin.ignore();
    
    cin >> s.my_score; cin.ignore();
    for (int i = 0; i < gargoyles_per_player; i++) {
        cin >> all_my_gargoyles[i].pos.x >> all_my_gargoyles[i].pos.y >> all_my_gargoyles[i].cooldown; cin.ignore();
    }

    cin >> s.opp_score; cin.ignore();
    for (int i = 0; i < gargoyles_per_player; i++) {
        cin >> all_opp_gargoyles[i].pos.x >> all_opp_gargoyles[i].pos.y >> all_opp_gargoyles[i].cooldown; cin.ignore();
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

    all_my_gargoyles.resize(3);
    all_opp_gargoyles.resize(3);
    
    State curr_state;

    cin >> gargoyles_per_player; cin.ignore();
    read_loop_input(curr_state);

    AI ai;
    timer.reset();

    vector<Point> res = ai.getFirstAction(curr_state);
    for(auto &r : res) {
        cout << "FLY " << r << endl;
    }

    while(1) {
        read_loop_input(curr_state);

        curr_state.short_debug();
        for(auto &action : curr_state.legalDestinations_presentsPredict()) {
            cerr << action << " | ";
        }
        cerr << '\n';

        timer.reset();
        vector<Point> res = ai.getAction(curr_state);
        for(auto &r : res) {
            cout << "FLY " << r << endl;
        }
    }
}