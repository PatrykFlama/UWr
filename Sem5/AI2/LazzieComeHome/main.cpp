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



const int START_TIME_LIMIT = 1000;
const int TURN_TIME_LIMIT = 50;

const int MAX_W = 200+5;
const int MAX_H = 200+5;

const int WIDTH = MAX_W*2;
const int HEIGHT = MAX_H*2;

enum DIR {N, S, W, E};
const Point DIR_POINT[] = {Point(0, -1), Point(0, 1), Point(-1, 0), Point(1, 0)};

char grid[HEIGHT][WIDTH];
int vision_range;
int home_distance_horizontal, home_distance_vertical;
Point pos, home;


void init() {
    pos = Point(HEIGHT/2, WIDTH/2);

    for(int i = 0; i < HEIGHT; i++) {
        for(int j = 0; j < WIDTH; j++) {
            grid[i][j] = '?';
        }
    }

    cin >> vision_range >> home_distance_horizontal >> home_distance_vertical;
    cin.ignore();

    home = Point(pos.x + home_distance_horizontal, pos.y + home_distance_vertical);
}

void read_vision() {
    for(int i = 0; i < vision_range; i++) {
        string row;
        cin >> row;
        for(int j = 0; j < vision_range; j++) {
            if(row[j] != '?' && row[j] != 'L') {
                grid[i+pos.y-vision_range/2][j+pos.x-vision_range/2] = row[j];
            };
        }
    }
}


int vis[HEIGHT][WIDTH];
int vis_cnt = 1;

Point parent[HEIGHT][WIDTH];

int g[HEIGHT][WIDTH];
int looked[HEIGHT][WIDTH];
int looked_cnt = 1;

inline int heuristic(const Point& p) {
    return p.manhattan_dist(home);
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(0);

    init();

    while(1) {
        Timer timer;
        read_vision();

        vector<Point> path;
        const auto comp = [](const pair<int, Point>& a, const pair<int, Point>& b) {
            return a.first > b.first;
        };
        priority_queue<pair<int, Point>, vector<pair<int, Point>>, decltype(comp)> pq(comp);

        pq.push({0, pos});
        vis_cnt++;
        looked_cnt++;
        g[pos.y][pos.x] = 0;
        parent[pos.y][pos.x] = pos;
        looked[pos.y][pos.x] = looked_cnt;

        while(!pq.empty()) {
            Point v = pq.top().second;
            pq.pop();

            if(v == home) {
                while(v != pos) {
                    path.push_back(v);
                    v = parent[v.y][v.x];
                }
                break;
            }

            if(vis[v.y][v.x] == vis_cnt) {
                continue;
            }
            vis[v.y][v.x] = vis_cnt;

            for(int i = 0; i < 4; i++) {
                const Point u = v + DIR_POINT[i];
                if(u.x < 0 || u.x >= WIDTH || u.y < 0 || u.y >= HEIGHT || grid[u.y][u.x] == '#') {
                    continue;
                }

                if(vis[u.y][u.x] == vis_cnt || (looked[u.y][u.x] == looked_cnt && g[u.y][u.x] <= g[v.y][v.x] + 1)) {
                    continue;
                }

                g[u.y][u.x] = g[v.y][v.x] + 1;
                parent[u.y][u.x] = v;
                looked[u.y][u.x] = looked_cnt;
                pq.push({g[u.y][u.x] + heuristic(u), u});
            }
        }

        Point next = path.back();
        path.pop_back();

        if(next.x > pos.x) {
            cout << "E";
            pos = pos + Point(1, 0);
        } else if(next.x < pos.x) {
            cout << "W";
            pos = pos + Point(-1, 0);
        } else if(next.y > pos.y) {
            cout << "S";
            pos = pos + Point(0, 1);
        } else if(next.y < pos.y) {
            cout << "N";
            pos = pos + Point(0, -1);
        }

        // if debug print 9 more
        if(DEBUG) {
            Point temp = pos;
            cerr << grid[temp.y][temp.x];
            for(int i = 0; i < 9 && !path.empty(); i++) {
                Point next = path.back();
                path.pop_back();
                if(next.x > pos.x) {
                    cout << "E";
                    temp = temp + Point(1, 0);
                } else if(next.x < pos.x) {
                    cout << "W";
                    temp = temp + Point(-1, 0);
                } else if(next.y > pos.y) {
                    cout << "S";
                    temp = temp + Point(0, 1);
                } else if(next.y < pos.y) {
                    cout << "N";
                    temp = temp + Point(0, -1);
                }
                cerr << grid[temp.y][temp.x];
            }
        }

        cout << endl;
    }
}