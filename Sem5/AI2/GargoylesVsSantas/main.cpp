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

    int turns_left;
    int missed_presents_to_end;

    State() : my_score(0), opp_score(0), turns_left(TURNS), missed_presents_to_end(MISSED_PRESENTS) {}

    bool isTerminal() const {
        return turns_left == 0 || missed_presents_to_end == 0;
    }

    int eval() const {
        return my_score - opp_score;
    }

    void applyAction(const Point &my_action, const Point &opp_action) {
        my_gargoyle.pos = my_action;
        opp_gargoyle.pos = opp_action;

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
    }

    vector<Point> legalActionsSingle(const Point &gargoyle_pos) const {
        vector<Point> actions;

        const int step = GARGOYLE_SPEED / 10;
        for(int nx = gargoyle_pos.x - GARGOYLE_SPEED; nx <= gargoyle_pos.x + GARGOYLE_SPEED; nx += step) {
            for(int ny = gargoyle_pos.y - GARGOYLE_SPEED; ny <= gargoyle_pos.y + GARGOYLE_SPEED; ny += step) {
                if(gargoyle_pos.int_dist({nx, ny}) > GARGOYLE_SPEED*GARGOYLE_SPEED || 
                   nx < 0 || nx >= WIDTH || ny < 0 || ny >= HEIGHT) continue;
                actions.push_back({nx, ny});
            }
        }

        return actions;
    }

    pair<vector<Point>, vector<Point>> legalActions() const {
        return {legalActionsSingle(my_gargoyle.pos), legalActionsSingle(opp_gargoyle.pos)};
    }
};


class Node {
public:
    State state;
    Node *parent;
    vector<Node*> children;
    vector<Point> to_vis;
    int visits = 0;
    double reward = 0.0;

    Node(State s, Node *p = nullptr) : state(s), parent(p) {
        to_vis = state.legalActions().first;
    }

    bool isFullyExpanded() const {
        const auto &actions = state.legalActions();
        return children.size() == actions.first.size() * actions.second.size();
        // return to_vis.empty();
    }

    bool isTerminal() {
        return state.isTerminal();
    }

    Node *bestChild(double explorationWeight = 1.0) const {
        Node *best = nullptr;
        double bestValue = -numeric_limits<double>::infinity();

        for (Node *child : children) {
            double uctValue = child->reward / (child->visits + 1e-6) +
                            explorationWeight * sqrt(log(visits + 1) / (child->visits + 1e-6));
            if (uctValue > bestValue) {
                bestValue = uctValue;
                best = child;
            }
        }

        return best;
    }
};


class MCTS {
    Node *root;
public:
    MCTS(State &rootState) {
        root = new Node(rootState);
    }

    // do we want that? it may kinda slow us down
    ~MCTS() {
        deleteTree(root);
    }
    void deleteTree(Node* node) {
        for (Node* child : node->children) {
            deleteTree(child);
        }
        delete node;
    }


    Node *expand(Node *node) {
        auto actions = node->state.legalActions();

        for (const auto &my_action : actions.first) {
            for (const auto &opp_action : actions.second) {
                bool alreadyExpanded = false;
                for (const auto &child : node->children) {
                    if (child->state.my_gargoyle.pos == my_action && child->state.opp_gargoyle.pos == opp_action) {
                        alreadyExpanded = true;
                        break;
                    }
                }

                if (!alreadyExpanded) {
                    State newState = node->state;
                    newState.applyAction(my_action, opp_action);
                    Node *newNode = new Node(newState, node);
                    node->children.push_back(newNode);
                    return newNode;
                }
            }
        }

        return nullptr;
    }

    double simulate(State state) {
        while (!state.isTerminal()) {
            auto actions = state.legalActions();
            state.applyAction(actions.first[rand() % actions.first.size()], actions.second[rand() % actions.second.size()]);
        }
        return state.eval();
    }

    void backpropagate(Node *node, double reward) {
        while (node != nullptr) {
            node->visits++;
            node->reward += reward;
            reward = -reward;
            node = node->parent;
        }
    }

    void move(State &state) {
        for (auto &child : root->children) {
            if (child->state.my_gargoyle.pos == state.my_gargoyle.pos) {
                root = child;
                root->parent = nullptr;
                return;
            }
        }

        root = new Node(state);
    }

    Point mcts(int time_limit_ms) {
        while(timer.elapsed() < time_limit_ms) {
            Node *node = root;

            // Selection
            while (!node->children.empty() && node->isFullyExpanded()) {
                node = node->bestChild();
            }

            // Expansion
            if (!node->state.isTerminal()) {
                node = expand(node);
            }

            // Simulation
            double reward = simulate(node->state);

            // Backpropagation
            backpropagate(node, reward);
        }

        Node *bestChild = root->bestChild(0.0); // Brak eksploracji przy wyborze ostatecznego ruchu
        return bestChild->state.my_gargoyle.pos;
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
        cin >> id >> x >> y >> value >> vy; cin.ignore();
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

    timer.reset();
    MCTS mcts(curr_state);

    Point res = mcts.mcts(1000);
    cout << "FLY " << res << endl;

    while (1) {
        read_loop_input(curr_state);
        mcts.move(curr_state);

        timer.reset();
        Point res = mcts.mcts(50);
        cout << "FLY " << res << endl;
    }
}