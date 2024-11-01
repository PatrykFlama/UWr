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

#define cerr if(1) cerr


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

    bool operator<(const Point& p) const {
        return x < p.x || (x == p.x && y < p.y);
    }

    int int_dist(const Point& p) const {
        return (x - p.x) * (x - p.x) + (y - p.y) * (y - p.y);
    }

    double dist(const Point& p) const {
        return sqrt(int_dist(p));
    }

    friend ostream& operator<<(ostream& os, const Point &p) {
        os << p.x << " " << p.y;
        return os;
    }
};

/* #endregion */


const double EPS = 1e-9;
const double SQ2 = sqrt(2);

const int TIME_LIMIT_MS = 1000 * 10;

const char WALL = '#';
const char TILE = '.';

const Point DIR[] = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}, {1, 1}, {1, -1}, {-1, 1}, {-1, -1}};
const double DIR_COST[] = {1, 1, 1, 1, SQ2, SQ2, SQ2, SQ2};


int landmarks_num;
float efficiency;
int width;
int height;

int avg_pairs_tested = 0;

inline bool point_safe(const Point& p) {
    return p.x >= 0 && p.x < width && p.y >= 0 && p.y < height;
}


class Landmark {
public:
    Point pos;
    vector<vector<double>> distances;

    Landmark(Point pos) : pos(pos) {
        distances.resize(height, vector<double>(width, INT_MAX));
    }

    void calculate_distances(vector<string> &grid) {
        // BFS
        queue<Point> q;
        q.push(pos);
        vector<vector<bool>> visited(height, vector<bool>(width, false));
        visited[pos.y][pos.x] = true;
        distances[pos.y][pos.x] = 0;

        while (!q.empty()) {
            Point p = q.front();
            q.pop();

            for (int i = 0; i < 8; i++) {
                Point new_p = p + DIR[i];

                // test if move is safe
                if (point_safe(new_p) == false)
                    continue;

                // test if collided or visited
                if (grid[new_p.y][new_p.x] == WALL || visited[new_p.y][new_p.x])
                    continue;

                distances[new_p.y][new_p.x] = distances[p.y][p.x] + DIR_COST[i];
                visited[new_p.y][new_p.x] = true;
                q.push(new_p);
            }
        }
    }
};


class Map {
public:
    vector<string> grid;
    vector<vector<Point>> tiles_in_cc;  //? [connected component][tile]

    Map() {
        grid.resize(height);
        parse_grid();
        find_tiles_in_cc();
        // debug();
    }

    void debug() {
        cerr << "Map found tiles in CC::\n";
        for (int i = 0; i < tiles_in_cc.size(); i++) {
            cerr << "CC " << i << " (" << tiles_in_cc[i].size() << "): ";
            for (const Point& p : tiles_in_cc[i]) {
                cerr << p << ", ";
            }
            cerr << '\n';
        }
        cerr << '\n';
    }

    void parse_grid() {
        for (int i = 0; i < height; i++) {
            string row; // A single row of the map consisting of passable terrain ('.') and walls ('#')
            cin >> row; cin.ignore();
            grid[i] = row;
        }
    }

    // find connected components, and store tiles in each connected component
    void find_tiles_in_cc() {
        vector<vector<bool>> visited(height, vector<bool>(width, false));

        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                if (grid[y][x] == WALL || visited[y][x]) {
                    continue;
                }

                tiles_in_cc.push_back(vector<Point>());

                queue<Point> q;
                q.push(Point(x, y));
                visited[y][x] = true;

                while (!q.empty()) {
                    Point p = q.front();
                    q.pop();

                    tiles_in_cc.back().push_back(p);

                    for (int k = 0; k < 4; k++) {
                        Point new_p = p + DIR[k];

                        if (point_safe(new_p) == false)
                            continue;

                        if (grid[new_p.y][new_p.x] == WALL || visited[new_p.y][new_p.x])
                            continue;

                        visited[new_p.y][new_p.x] = true;
                        q.push(new_p);
                    }
                }
            }
        }
    }

    // ---- random tile functions ----
    inline Point random_tile() const {
        const int cc = rand() % tiles_in_cc.size();
        const int tile = rand() % tiles_in_cc[cc].size();
        return tiles_in_cc[cc][tile];
    }

    inline Point random_tile_from_cc(int cc) const {
        const int tile = rand() % tiles_in_cc[cc].size();
        return tiles_in_cc[cc][tile];
    }

    inline int random_cc() const {
        return rand() % tiles_in_cc.size();
    }
};


class Solution {
    int vis_cnt = 1;
    vector<vector<int>> visited;
public:
    Map map;

    Solution() : map() {
        visited.resize(height, vector<int>(width, 0));
    }

    inline double heuristic(const Point& p, const Point& end, vector<Landmark> &lms) const {
        double h = 0;
        for (int i = 0; i < landmarks_num; i++) {
            h = max(h, abs(lms[i].distances[p.y][p.x] - lms[i].distances[end.y][end.x]));
        }
        return h;
    }

    // ----- test solution -----  
    double test_solution(vector<Landmark> &lms, int t = 100) {
        Timer timer;

        int pairs_tested = 0;
        // int debug_lines = 10;
        double efficiency = 1e9;

        while (timer.elapsed() < t) {
            pairs_tested++;

            const int cc = map.random_cc();
            const Point start = map.random_tile_from_cc(cc);
            const Point end = map.random_tile_from_cc(cc);

            // find path using A* with landmarks
            int n_visited = 0;
            double path_length = 0;

            auto comp = [](const pair<pair<double, double>, Point>& a, const pair<pair<double, double>, Point>& b) {
                return a.first.first > b.first.first;
            };

            priority_queue<pair<pair<double, double>, Point>, 
                            vector<pair<pair<double, double>, Point>>, 
                            decltype(comp)> pq(comp);     // {{heuristic value, real_cost}, point}

            pq.push({{0, 0}, start});
            vis_cnt++;

            while (!pq.empty()) {
                Point p = pq.top().second;
                auto [h_cost, cost] = pq.top().first;
                pq.pop();

                if (p == end) {
                    path_length = cost;
                    break;
                }

                if (visited[p.y][p.x] == vis_cnt)
                    continue;

                visited[p.y][p.x] = vis_cnt;
                n_visited++;

                for (int i = 0; i < 8; i++) {
                    const Point new_p = p + DIR[i];

                    if(point_safe(new_p) == false)
                        continue;

                    if (map.grid[new_p.y][new_p.x] == WALL || visited[new_p.y][new_p.x] == vis_cnt)
                        continue;

                    const double new_cost = cost + DIR_COST[i];
                    const double h = heuristic(new_p, end, lms);
                    pq.push({{new_cost + h, new_cost}, new_p});
                }
            }

            efficiency = min(efficiency, path_length / (double)n_visited);
        }

        if(!avg_pairs_tested) avg_pairs_tested = pairs_tested;
        pairs_tested = (pairs_tested + avg_pairs_tested) / 2;

        return efficiency;
    }

    // ------ solvers ------
    int active_landmarks_function = 0;
    inline vector<Landmark>* active_random_landmarks_function() {
        active_landmarks_function++;

        if(active_landmarks_function%4)
            return gen_landmarks_furthest();
        return gen_landmarks_random();
    }

    vector<Landmark>* gen_landmarks_random() {
        vector<Landmark> *lms = new vector<Landmark>();

        for (int i = 0; i < landmarks_num; i++) {
            lms->emplace_back(map.random_tile());
            lms->back().calculate_distances(map.grid);
        }

        return lms;
    }

    vector<Landmark>* gen_landmarks_furthest() {
        vector<Landmark> *lms = new vector<Landmark>();

        Point first_landmark = map.random_tile();
        lms->emplace_back(first_landmark);
        lms->back().calculate_distances(map.grid);

        for (int i = 1; i < landmarks_num; i++) {
            Point furthest_point;
            double max_dist = -1;

            for (const auto& row : map.tiles_in_cc) {
                for (const auto& tile : row) {
                    double min_dist = INT_MAX;
                    for (const auto& lm : *lms) {
                        min_dist = min(min_dist, lm.distances[tile.y][tile.x]);
                    }
                    if (min_dist > max_dist) {
                        max_dist = min_dist;
                        furthest_point = tile;
                    }
                }
            }

            lms->emplace_back(furthest_point);
            lms->back().calculate_distances(map.grid);
        }

        return lms;
    }

    // tests random landmarks and returns the estimated best set of landmarks
    vector<Landmark>* solve(int t = TIME_LIMIT_MS, int solution_testing_time = 100) {
        Timer timer;
        cerr << "Do i even care?\n"; 

        int landmarks_tested = 0;

        vector<Landmark> *best_lms = new vector<Landmark>();
        double best_efficiency = 0;

        while (timer.elapsed() < t) {
            landmarks_tested++;

            vector<Landmark> *lms = active_random_landmarks_function();

            const double efficiency = test_solution(*lms, solution_testing_time);

            if (efficiency > best_efficiency) {
                best_efficiency = efficiency;
                delete best_lms;
                best_lms = lms;
            } else {
                delete lms;
            }
        }

        cerr << "Tested " << landmarks_tested << " sets of landmarks\n";
        cerr << "Estimated efficiency: " << best_efficiency << '\n';
        cerr << "Average pairs tested per set: " << avg_pairs_tested << '\n';

        return best_lms;
    }
};

int main() {
    cin >> landmarks_num >> efficiency; cin.ignore();
    cin >> width >> height; cin.ignore();

    Solution s;

    vector<Landmark> *lms = s.solve(9000, 30);

    for (int i = 0; i < landmarks_num; i++) {
        // cout << (*lms)[i].pos.y << ' ' << (*lms)[i].pos.x << '\n';
        cout << (*lms)[i].pos.x << ' ' << (*lms)[i].pos.y << '\n';
    }
    cout << endl;

    delete lms;
}
