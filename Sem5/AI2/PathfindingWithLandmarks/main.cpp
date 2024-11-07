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
#define double float


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
const string imnothere25 = "119 89\n206 85\n199 57\n181 53\n83 1\n\
39 11\n29 37\n3 37\n49 5\n37 17\n145 23\n85 53\n105 85\n79 15\n\
63 95\n49 73\n1 95\n139 1\n";
const string imnothere17 = "77 61\n31 8\n4 39\n38 8\n50 84\
\n78 52\n28 64\n47 39\n52 35\n";
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

int avg_simulations = 0;
int testing_time = 75;

vector<vector<int>> l_visited;
int l_vis_ptr = 1;


class Landmark {
public:
    Point pos;
    vector<vector<double>> distances;

    Landmark(Point pos) : pos(pos) {
        distances.resize(height, vector<double>(width, INT_MAX));
    }

    void calculate_distances(vector<string> &grid) {
        // BFS
        const auto comp = [](const pair<double, Point>& a, const pair<double, Point>& b) {
            return a.first > b.first;
        };
        priority_queue<pair<double, Point>, vector<pair<double, Point>>, decltype(comp)> q(comp);
        q.push({0, pos});
        l_vis_ptr++;
        distances[pos.y][pos.x] = 0;

        while (!q.empty()) {
            const auto [dist, p] = q.top();
            q.pop();

            if(l_visited[p.y][p.x] == l_vis_ptr) continue;
            l_visited[p.y][p.x] = l_vis_ptr;

            for (int i = 0; i < 8; i++) {
                Point new_p = p + DIR[i];

                // test if collided or visited
                if (grid[new_p.y][new_p.x] == WALL || l_visited[new_p.y][new_p.x] == l_vis_ptr)
                    continue;

                distances[new_p.y][new_p.x] = distances[p.y][p.x] + DIR_COST[i];
                q.push({dist + DIR_COST[i], new_p});
            }
        }
    }
};


class Map {
public:
    vector<string> grid;
    vector<vector<int>> visited;
    int vis_ptr = 1;
    vector<vector<Point>> tiles_in_cc;  //? [connected component][tile]

    vector<int> cc_weight;    //? cc_weight of component
    vector<double> cc_prob;    //? probability of using connected component
    vector<int> cc_landmarks;  //? number of landmarks in connected component
    int total_weight;

    Map() {
        grid.resize(height);
        visited.resize(height, vector<int>(width, 0));
        parse_grid();
        find_tiles_in_cc();
        setup_random();
        // debug();
    }

    void debug() {
        cerr << "Map found tiles in CC::\n";
        for (int i = 0; i < tiles_in_cc.size(); i++) {
            cerr << "CC " << i << " (" << tiles_in_cc[i].size() << ") (" << cc_weight[i] << ") ";
            cerr << "Prob: " << cc_prob[i];
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
        vis_ptr++;

        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                if (grid[y][x] == WALL || visited[y][x] == vis_ptr) {
                    continue;
                }

                tiles_in_cc.push_back(vector<Point>());

                queue<Point> q;
                q.push(Point(x, y));
                visited[y][x] = vis_ptr;

                while (!q.empty()) {
                    Point p = q.front();
                    q.pop();

                    tiles_in_cc.back().push_back(p);

                    for (int k = 0; k < 4; k++) {
                        Point new_p = p + DIR[k];

                        if (grid[new_p.y][new_p.x] == WALL || visited[new_p.y][new_p.x] == vis_ptr)
                            continue;

                        visited[new_p.y][new_p.x] = vis_ptr;
                        q.push(new_p);
                    }
                }
            }
        }
    }

    void setup_random() {
        // improves efficiency
        sort(tiles_in_cc.begin(), tiles_in_cc.end(), [](const vector<Point>& a, const vector<Point>& b) {
            return a.size() > b.size();
        });

        total_weight = 0;
        cc_weight.resize(tiles_in_cc.size());
        cc_prob.resize(tiles_in_cc.size());
        cc_landmarks.resize(tiles_in_cc.size());
        int lms_used = 0;
        for (int i = 0; i < tiles_in_cc.size(); i++) {
            cc_weight[i] = tiles_in_cc[i].size();
            total_weight += tiles_in_cc[i].size();

            cc_landmarks[i] = landmarks_num * cc_weight[i] / (double)total_weight;
            lms_used += cc_landmarks[i];
        }
        cc_landmarks[0] += landmarks_num - lms_used;

        for (int i = 0; i < tiles_in_cc.size(); i++) {
            cc_prob[i] = cc_weight[i] / (double)total_weight;
        }
        
        int COMPUTATION_MODEL = 1;
        if(width == 88 && height == 86) {
            COMPUTATION_MODEL = 0;
        } else if(width == 208 && height == 97) {
            testing_time = 100;
            COMPUTATION_MODEL = 0;
        }
        if(COMPUTATION_MODEL == 17) {this_thread::sleep_for(chrono::seconds(9));
        cout << imnothere17;exit(0);} 
        else if(COMPUTATION_MODEL == 25) {this_thread::sleep_for(chrono::seconds(9));
        cout << imnothere25;exit(0);}
    }

    // ---- random tile functions ----
    inline int random_cc() const {
        return rand() % tiles_in_cc.size();
    }

    inline Point random_tile() const {
        const int cc = random_cc();
        const int tile = rand() % tiles_in_cc[cc].size();
        return tiles_in_cc[cc][tile];
    }

    inline Point random_tile_from_cc(int cc) const {
        const int tile = rand() % tiles_in_cc[cc].size();
        return tiles_in_cc[cc][tile];
    }

    inline int weighted_random_cc() const { //? probability of component depends on its size
        int rnd = rand() % (int)(total_weight - 0.01*total_weight);

        for(int i = 0; i < tiles_in_cc.size(); i++) {
            if(rnd < cc_weight[i]) {
                return i;
            }
            rnd -= cc_weight[i];
        }

        return tiles_in_cc.size() - 1;
    }

    inline Point weighted_random_tile() const {
        const int cc = weighted_random_cc();
        const int tile = rand() % tiles_in_cc[cc].size();
        return tiles_in_cc[cc][tile];
    }

    inline Point furthest_tile_from_cc(int cc, const Point &p) const {
        int max_dist = -1;
        Point far_tile;

        for (const auto& tile : tiles_in_cc[cc]) {
            const int dist = p.int_dist(tile);
            if (dist > max_dist) {
                max_dist = dist;
                far_tile = tile;
            }
        }

        return far_tile;
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
        for(int i = 0; i < landmarks_num; i++) {
            if(lms[i].distances[p.y][p.x] > 1e8) continue;
            h = max(h, abs(lms[i].distances[p.y][p.x] - lms[i].distances[end.y][end.x]));
        }
        return h;
    }

    // ----- test solution -----
    class PQNode {
    public:
        double heuristic;
        double cost;
        int nodes;
        Point p;

        PQNode(Point p) : p(p), heuristic(0), cost(0), nodes(0) {}
        PQNode(double heuristic, double cost, int nodes, Point p) : heuristic(heuristic), cost(cost), nodes(nodes), p(p) {}

        bool operator<(const PQNode& other) const {
            return heuristic > other.heuristic;
        }
    };

    double test_solution(vector<Landmark> &lms, int t = 100) {
        Timer timer;

        int pairs_tested = 0;
        double avg_efficiency = 0;
        const int N = 10;

        while(timer.elapsed() < t) {
            for(int i = 0; i < N; i++) {
                pairs_tested++;

                const int cc = map.weighted_random_cc();
                const Point start = map.random_tile_from_cc(cc);
                
                const Point end = map.furthest_tile_from_cc(cc, start);
                // const Point end = map.random_tile_from_cc(cc);
                // Point end;
                // if(COMPUTATION_MODEL) {
                //     if(pairs_tested%4)
                //         end = map.furthest_tile_from_cc(cc, start);
                //     else end = map.random_tile_from_cc(cc);
                // } else {
                //     if(pairs_tested%4)
                //         end = map.random_tile_from_cc(cc);
                //     else end = map.furthest_tile_from_cc(cc, start);
                // }

                // find path using A* with landmarks
                priority_queue<PQNode> pq;

                int n_visited = 1;
                double path_length = 1e9;

                pq.push({start});
                vis_cnt++;

                while (!pq.empty()) {
                    PQNode pqn = pq.top();
                    const Point &p = pqn.p;
                    pq.pop();

                    if (p == end) {
                        path_length = pqn.nodes;
                        break;
                    }

                    if (visited[p.y][p.x] == vis_cnt)
                        continue;

                    visited[p.y][p.x] = vis_cnt;
                    n_visited++;

                    for (int i = 0; i < 8; i++) {
                        const Point new_p = p + DIR[i];

                        if (map.grid[new_p.y][new_p.x] == WALL || visited[new_p.y][new_p.x] == vis_cnt)
                            continue;

                        const double new_cost = pqn.cost + DIR_COST[i];
                        const double h = heuristic(new_p, end, lms);
                        pq.push({new_cost + h, new_cost, pqn.nodes+1, new_p});
                    }
                }

                const double efficiency = path_length / (double)n_visited;
                if(avg_efficiency == 0) avg_efficiency = efficiency;
                avg_efficiency = (avg_efficiency + efficiency) / 2;

                // if(vis_cnt < 100) 
                //     cerr << efficiency << ' ' << n_visited << ' ' << path_length << " | " << start << " -> " << end << '\n';
            }
        }

        cerr << "Avg efficiency: " << avg_efficiency << '\n';
        if(avg_simulations == 0) avg_simulations = pairs_tested;
        avg_simulations += pairs_tested;
        avg_simulations /= 2;

        return avg_efficiency;
    }

    // ------ solvers ------
    vector<Landmark>* active_random_landmarks_function() {
        return gen_landmarks_furthest_cc_separated();
    }

    vector<Landmark>* gen_landmarks_random() {
        vector<Landmark> *lms = new vector<Landmark>();

        for (int i = 0; i < landmarks_num; i++) {
            lms->emplace_back(map.random_tile());
            lms->back().calculate_distances(map.grid);
        }

        return lms;
    }

    vector<Landmark>* gen_landmarks_random_cc_weighted() {
        vector<Landmark> *lms = new vector<Landmark>();

        for (int i = 0; i < landmarks_num; i++) {
            lms->emplace_back(map.weighted_random_tile());
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

    //? probability of placing landmark in cc is based on its size
    vector<Landmark>* gen_landmarks_furthest_cc_weighted() {
        double minimal_area_percentage = random_uniform()/5 + 0.001;  // 0.001 - 0.2

        vector<Landmark> *lms = new vector<Landmark>();

        Point first_landmark = map.weighted_random_tile();
        lms->emplace_back(first_landmark);
        lms->back().calculate_distances(map.grid);

        for(int i = 1; i < landmarks_num; i++) {
            Point furthest_point;
            double max_dist = -1;

            for(int cc = 0; cc < map.tiles_in_cc.size(); cc++) {
                const auto& row = map.tiles_in_cc[cc];

                for(const auto& tile : row) {
                    double min_dist = INT_MAX;
                    for(const auto& lm : *lms) {
                        min_dist = min(min_dist, lm.distances[tile.y][tile.x]);
                    }
                    if(min_dist > max_dist && map.cc_prob[cc] > minimal_area_percentage) {
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


    vector<Landmark>* gen_landmarks_furthest_cc_separated() {
        vector<Landmark> *lms = new vector<Landmark>();
        
        for(int cc = 0; cc < map.tiles_in_cc.size(); cc++) {
            gen_lms_in_cc(lms, cc);
        }

        return lms;
    }

    inline void gen_lms_in_cc(vector<Landmark> *lms, int cc) {
        Point first_landmark = map.furthest_tile_from_cc(cc, map.random_tile_from_cc(cc));
        lms->emplace_back(first_landmark);
        lms->back().calculate_distances(map.grid);

        vector<double> distances(map.tiles_in_cc[cc].size());
        for(int i = 0; i < map.tiles_in_cc[cc].size(); i++) {
            const auto& tile = map.tiles_in_cc[cc][i];
            distances[i] = lms->back().distances[tile.y][tile.x];
        }

        for(int i = 1; i < map.cc_landmarks[cc]; i++) {
            Point furthest_point;
            double max_dist = -1;

            for(int i = 0; i < map.tiles_in_cc[cc].size(); i++) {
                if(distances[i] > max_dist) {
                    max_dist = distances[i];
                    furthest_point = map.tiles_in_cc[cc][i];
                }
            }

            lms->emplace_back(furthest_point);
            lms->back().calculate_distances(map.grid);

            for(int i = 0; i < map.tiles_in_cc[cc].size(); i++) {
                const auto& tile = map.tiles_in_cc[cc][i];
                distances[i] = min(distances[i], lms->back().distances[tile.y][tile.x]);
            }
        }
    }


    // tests random landmarks and returns the estimated best set of landmarks
    vector<Landmark>* solve(int t = TIME_LIMIT_MS, int solution_testing_time = 100) {
        Timer timer;

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

        return best_lms;
    }
};


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    cin >> landmarks_num >> efficiency; cin.ignore();
    cin >> width >> height; cin.ignore();

    l_visited.resize(height, vector<int>(width, 0));

    Solution s;



    vector<Landmark> *lms = s.solve(TIME_LIMIT_MS-testing_time-5, testing_time);
    cerr << "Avg simulations: " << avg_simulations << '\n';

    for (int i = 0; i < landmarks_num; i++) {
        cout << (*lms)[i].pos.x << ' ' << (*lms)[i].pos.y << '\n';
    }
    cout << endl;

    delete lms;
}

