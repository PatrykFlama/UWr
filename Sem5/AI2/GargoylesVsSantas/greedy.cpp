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

int gargoyles_per_player; // liczba gargulców w drużynie
Timer timer;


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

    Gargoyle my_gargoyle;
    Gargoyle opp_gargoyle;
    int my_score;
    int opp_score;

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
        return (is_my_turn ? 1 : -1) * (my_score - opp_score);
        // return my_score - opp_score;
    }

    Point get_main_player_pos() const {
        return is_my_turn ? my_gargoyle.pos : opp_gargoyle.pos;
    }

    void swap_roles() {
        swap(my_gargoyle, opp_gargoyle);
        swap(my_score, opp_score);
        is_my_turn = !is_my_turn;
    }

    void applyAction(const Point &my_action) {
        my_gargoyle.pos = my_action;

        if(is_my_turn) {
            swap_roles();
            return;
        }

        list<Present>::iterator present = presents.begin();
        while(present != presents.end()) {
            const int my_points = (my_gargoyle.pos.int_dist(present->pos) <= 30*30) ? present->value : 0;
            const int opp_points = (opp_gargoyle.pos.int_dist(present->pos) <= 30*30) ? present->value : 0;

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

    vector<Point> legalActions() const {
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

    friend ostream& operator<<(ostream& os, const State &s) {
        os << "My score: " << s.my_score << '\n';
        os << "Opp score: " << s.opp_score << '\n';
        os << "My gargoyle: " << s.my_gargoyle.pos << " " << s.my_gargoyle.cooldown << '\n';
        os << "Opp gargoyle: " << s.opp_gargoyle.pos << " " << s.opp_gargoyle.cooldown << '\n';
        os << "My turn: " << (s.is_my_turn ? "true" : "false") << '\n';
        os << "Presents: " << '\n';
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


void read_loop_input(State &s) {
    cin >> s.missed_presents_to_end; cin.ignore();
    
    cin >> s.my_score; cin.ignore();
    for (int i = 0; i < gargoyles_per_player; i++) {
        cin >> s.my_gargoyle.pos.x >> s.my_gargoyle.pos.y >> s.my_gargoyle.cooldown; cin.ignore();
    }

    cin >> s.opp_score; cin.ignore();
    for (int i = 0; i < gargoyles_per_player; i++) {
        cin >> s.opp_gargoyle.pos.x >> s.opp_gargoyle.pos.y >> s.opp_gargoyle.cooldown; cin.ignore();
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

Point get_closest_dest(State &state) {
    const int tolerance = 30;
    // Point res = state.my_gargoyle.pos;
    Point res = {WIDTH / 2, HEIGHT / 2};
    int best_rounds = INT_MAX;
    for(const Present &p : state.presents) {
        for(int round = 1; round < 20; round++) {
            for(int tol = -tolerance; tol <= tolerance; tol += 10) {
                Point new_pos = {p.pos.x + tol, p.pos.y - p.vy * round};
                if(new_pos.y < 0) break;

                int rounds = (int)ceil(state.my_gargoyle.pos.dist(new_pos) / GARGOYLE_SPEED);
                if(rounds == round && rounds < best_rounds) {
                    best_rounds = rounds;
                    res = new_pos;
                    break;
                }
            }

            for(int tol = -tolerance; tol <= tolerance; tol += 10) {
                Point new_pos = {p.pos.x, p.pos.y - p.vy * round + tol};
                if(new_pos.y < 0) break;

                int rounds = (int)ceil(state.my_gargoyle.pos.dist(new_pos) / GARGOYLE_SPEED);
                if(rounds == round && rounds < best_rounds) {
                    best_rounds = rounds;
                    res = new_pos;
                    break;
                }
            }
        }
    }

    return res;
}


Point get_closest_opponent_aware(State &state) {
    const int tolerance = 30;
    Point res = {0, 0};
    int best_rounds = INT_MAX;

    const Point &my_pos = state.my_gargoyle.pos;
    const Point &opp_pos = state.opp_gargoyle.pos;

    for(const Present &p : state.presents) {
        int my_rounds = INT_MAX;
        int opp_rounds = INT_MAX;

        Point my_res = {WIDTH / 2, HEIGHT / 2};
        Point opp_res = {WIDTH / 2, HEIGHT / 2};

        for(int round = 1; round < 20; round++) {
            for(int tol = -tolerance; tol <= tolerance; tol += 10) {
                Point new_pos = {p.pos.x + tol, p.pos.y - p.vy * round};
                if(new_pos.y < 0) break;

                int rounds = (int)ceil(my_pos.dist(new_pos) / GARGOYLE_SPEED);
                if(rounds == round && rounds < my_rounds) {
                    my_rounds = rounds;
                    my_res = new_pos;
                    break;
                }
            }

            for(int tol = -tolerance; tol <= tolerance; tol += 10) {
                Point new_pos = {p.pos.x, p.pos.y - p.vy * round + tol};
                if(new_pos.y < 0) break;

                int rounds = (int)ceil(my_pos.dist(new_pos) / GARGOYLE_SPEED);
                if(rounds == round && rounds < my_rounds) {
                    my_rounds = rounds;
                    my_res = new_pos;
                    break;
                }
            }
        }

        for(int round = 1; round < 20; round++) {
            for(int tol = -tolerance; tol <= tolerance; tol += 10) {
                Point new_pos = {p.pos.x + tol, p.pos.y - p.vy * round};
                if(new_pos.y < 0) break;

                int rounds = (int)ceil(opp_pos.dist(new_pos) / GARGOYLE_SPEED);
                if(rounds == round && rounds < opp_rounds) {
                    opp_rounds = rounds;
                    opp_res = new_pos;
                    break;
                }
            }

            for(int tol = -tolerance; tol <= tolerance; tol += 10) {
                Point new_pos = {p.pos.x, p.pos.y - p.vy * round + tol};
                if(new_pos.y < 0) break;

                int rounds = (int)ceil(opp_pos.dist(new_pos) / GARGOYLE_SPEED);
                if(rounds == round && rounds < opp_rounds) {
                    opp_rounds = rounds;
                    opp_res = new_pos;
                    break;
                }
            }
        }

        if(my_rounds <= opp_rounds && my_rounds < best_rounds) {
            best_rounds = my_rounds;
            res = my_res;
        }
    }
}


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    srand(time(0));
    
    State curr_state;

    cin >> gargoyles_per_player; cin.ignore();

    while (1) {
        read_loop_input(curr_state);
        // cerr << curr_state << '\n';

        // find closest present
        // calculate in how lower it will be next round 
        // (calculate rounds needed to get to it, and how much it will fall)
        // if it's worth it, go for it
        // but if opponent is closer dont go for it

        Point res;
        
        // res = get_closest_dest2(curr_state);
        // if(res == Point(0, 0)) {
        //     res = get_closest_dest(curr_state);
        // }

        // res = get_closest_dest(curr_state);
        res = get_closest_opponent_aware(curr_state);
        if(res == Point(0, 0)) {
            res = get_closest_dest(curr_state);
        }
        
        cout << "FLY " << res << endl;
    }
}