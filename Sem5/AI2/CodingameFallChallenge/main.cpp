#include <bits/stdc++.h>
using namespace std;

#define pii pair<int, int>
#define cerr if(1)cerr

/*
IDEA
at the beginning of each month we are given all information required to simulate it 
thus we can have solution from the beginning of the month 
and transform it with our **modifications**, then simulate and test how good the modifications were
try as many modifications as possible and choose the best one

so we need:
- a class to hold the solution
- a way to generate modifications
- a way to simulate the solution

______ GREEDY
for each landing pad:
- build teleporter for the most common destination (at best to the further one, but not occupied already)
- build tube with pod to other destinations (at best to the closest one)
- if tube collides with some other tube:
    -- for all neighbouring (transportation-wise) buildings
    -- pick the one that has the shortest tube and
    -- upgrade tube to this building
    -- add one pod
*/

/* #region ENUMS */
const string MOVE_NAMES[] = {"TUBE", "UPGRADE", "TELEPORT", "POD", "WAIT"};
// tube 1 per 0.1km; initial cost of tube * new capacity; 5000; 1000; (+750); 0 
const int COSTS[] = {1, -1, 5000, 1000, 0};
enum MoveType {
    TUBE,       // buildingId1 buildingId2
    UPGRADE,    // buildingId1 buildingId2
    TELEPORT,   // buildingIdEntrance buildingIdExit
    POD,        // podId buildingId1 buildingId2 buildingId3 ...
    // DESTROY,    //  podId
    WAIT
};
/* #endregion */

/* #region STRUCTS */
class Building {
public:
    int id;
    pii pos;

    Building() {}
    Building(int id, pii pos) : id(id), pos(pos) {}
};

class LandingPad : public Building {
public:
    vector<pii> astronauts; // {type, amt}  sorted by amt

    LandingPad() {}
    LandingPad(int id, pii pos, vector<pii> astronauts) : Building(id, pos), astronauts(astronauts) {}
};

class Destination : public Building {
public:
    int type;   

    Destination() {}
    Destination(int id, pii pos, int type) : Building(id, pos), type(type) {}
};

class Pod {
public:
    int id;
    vector<int> stops; // ids of buildings

    Pod() {}
    Pod(int id, vector<int> stops) : id(id), stops(stops) {}
};

class Tube {
public:
    int id;
    int building1;
    int building2;
    int capacity;

    Tube() {}
    Tube(int id, int building1, int building2, int capacity) : id(id), building1(building1), building2(building2), capacity(capacity) {}
};

class Teleporter {
public:
    int id;
    int building_from;
    int building_to;

    Teleporter() {}
    Teleporter(int id, int building_from, int building_to) : id(id), building_from(building_from), building_to(building_to) {}
};

class Move {
public:
    MoveType type;
    vector<int> args;

    Move() {}
    Move(MoveType type, vector<int> args) : type(type), args(args) {}
};
/* #endregion */

class Solution {
public:
    int resources;
    unordered_map<int, Tube> tubes;
    unordered_map<int, Teleporter> teleporters;
    unordered_map<int, Pod> pods;
    vector<Destination> buildings;
    vector<LandingPad> pads;

    unordered_map<int, vector<pii>> graph;  // {fromBuildingID, [{toBuildingID, cost}]}
    vector<vector<int>> dist;  // dist[from][to] = cost

    Solution() {}
    Solution(int resources, vector<Tube> &tubes, vector<Teleporter> &teleporters, vector<Pod> &pods, vector<Destination> &buildings, vector<LandingPad> &pads) {
        this->resources = resources;
        this->buildings = buildings;
        this->pads = pads;

        for(Tube& tube : tubes) {
            this->tubes[tube.id] = tube;
        }
        for(Teleporter& teleporter : teleporters) {
            this->teleporters[teleporter.id] = teleporter;
        }
        for(Pod& pod : pods) {
            this->pods[pod.id] = pod;
        }

        for(Tube& tube : tubes) {
            graph[tube.building1].push_back({tube.building2, 1});
            graph[tube.building2].push_back({tube.building1, 1});
        }
        for(Teleporter& teleporter : teleporters) {
            graph[teleporter.building_from].push_back({teleporter.building_to, 0});
        }
    }

    void add_buildings(vector<Destination> &buildings, vector<LandingPad> &pads) {
        for(Destination& building : buildings) {
            this->buildings.push_back(building);
        }
        for(LandingPad& pad : pads) {
            this->pads.push_back(pad);
        }
    }

    void recalc_shortest() {
        // TODO floyd warshall does not have to be recalculated every time - new buildings can be simply added
        int n = buildings.size();
        dist = vector<vector<int>>(n, vector<int>(n, INT_MAX));
        for(int i = 0; i < n; i++) {
            dist[i][i] = 0;
            for(pii& p : graph[i]) {
                dist[i][p.first] = p.second;
            }
        }
        for(int k = 0; k < n; k++) {
            for(int i = 0; i < n; i++) {
                for(int j = 0; j < n; j++) {
                    dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j]);
                }
            }
        }
    }

    int eval() {    // some heuristic
        //! floyd warshall instead of dijkstra for easy recalc
        // from each landing pad check the best route (with time) to needed destination
        // then simulate the travelsal and calc score

        recalc_shortest();

        int score = 0;
        for(LandingPad& pad : pads) {
            for(pii& astronaut : pad.astronauts) {
                const int dest = astronaut.first;
                const int amt = astronaut.second;

                int best_time = 1e9;
                for(Destination& building : buildings) {
                    if(building.type == dest)
                        best_time = min(best_time, dist[building.id][dest]);
                }

                score += best_time * amt;   //! strong heuristic here (can be improved)
            }
        }

        return score;
    }


    friend ostream& operator<<(ostream& os, const Solution& sol) {
        os << "Solution: \n";
        os << "Resources: " << sol.resources << '\n';
        os << "Tubes: \n";
        for(auto& tube : sol.tubes) {
            os << tube.second.id << " " << tube.second.building1 << " " << tube.second.building2 << " " << tube.second.capacity << '\n';
        }
        os << "Teleporters: \n";
        for(auto& teleporter : sol.teleporters) {
            os << teleporter.second.id << " " << teleporter.second.building_from << " " << teleporter.second.building_to << '\n';
        }
        os << "Pods: \n";
        for(auto& pod : sol.pods) {
            os << pod.second.id << " ";
            for(int stop : pod.second.stops) {
                os << stop << " ";
            }
            os << '\n';
        }
        os << "Buildings: \n";
        for(const Destination& building : sol.buildings) {
            os << building.id << " " << building.pos.first << " " << building.pos.second << " " << building.type << '\n';
        }
        os << "Landing pads: \n";
        for(const LandingPad& pad : sol.pads) {
            os << pad.id << " " << pad.pos.first << " " << pad.pos.second << "\n";
            for(const pii& astronaut : pad.astronauts) {
                os << astronaut.first << " " << astronaut.second << " ";
            }
            os << '\n';
        }

        return os;
    }


    vector<Move> greedy_teleporters() {
        vector<Move> moves;
        for(LandingPad& pad : pads) {
            if(resources < COSTS[TELEPORT]) break;

            // check if pad already has teleporter
            bool has_teleporter = false;
            for(auto& teleporter : teleporters) {
                if(teleporter.second.building_from == pad.id) {
                    has_teleporter = true;
                    break;
                }
            }
            if(has_teleporter) continue;

            for(pii& astronaut : pad.astronauts) {
                bool has_building = false;
                for(Destination& building : buildings) {
                    if(building.type != astronaut.first) 
                        continue;

                    // check if building is occupied
                    bool is_occupied = false;
                    for(auto& teleporter : teleporters) {
                        if(teleporter.second.building_to == building.id || teleporter.second.building_from == building.id) {
                            is_occupied = true;
                            break;
                        }
                    }
                    if(is_occupied) continue;

                    moves.push_back({TELEPORT, {pad.id, building.id}});
                    teleporters[teleporters.size()*10] = {teleporters.size()*10, pad.id, building.id};
                    resources -= COSTS[TELEPORT];
                    has_building = true;
                    break;
                }
                if(has_building) break;
            }

            // find fitting building that is not occupied
        }

        return moves;
    }
};


void parse_input(Solution &sol) {
    int resources;
    cin >> resources; cin.ignore();

    int num_travel_routes;
    cin >> num_travel_routes; cin.ignore();
    vector<Tube> tubes;
    vector<Teleporter> teleporters;
    for (int i = 0; i < num_travel_routes; i++) {
        int building_id_1;
        int building_id_2;
        int capacity;
        cin >> building_id_1 >> building_id_2 >> capacity; cin.ignore();

        if(capacity == 0)
            teleporters.push_back({i, building_id_1, building_id_2});
        else
            tubes.push_back({i, building_id_1, building_id_2, capacity});
    }

    int num_pods;
    cin >> num_pods; cin.ignore();
    vector<Pod> pods;
    for (int i = 0; i < num_pods; i++) {
        int pod_id, num_stops;
        cin >> pod_id >> num_stops;
        vector<int> stops;
        for(int j = 0; j < num_stops; j++) {
            int stop;
            cin >> stop;
            stops.push_back(stop);
        }
        pods.push_back({pod_id, stops});
    }

    int num_new_buildings;
    cin >> num_new_buildings; cin.ignore();
    vector<Destination> buildings;
    vector<LandingPad> pads;
    for (int i = 0; i < num_new_buildings; i++) {
        int type, building_id;
        pii pos;
        cin >> type >> building_id >> pos.first >> pos.second;
        if(type == 0) { // landing pad
            int num_astronauts;
            cin >> num_astronauts;

            unordered_map<int, int> m_astronauts; // {type, amt}
            for(int j = 0; j < num_astronauts; j++) {
                int astronaut_type;
                cin >> astronaut_type;
                m_astronauts[astronaut_type]++;
            }

            // count unique astronauts
            vector<pii> astronauts;
            for(auto& astronaut : m_astronauts) {
                astronauts.push_back({astronaut.first, astronaut.second});
            }

            // sort by amount
            sort(astronauts.begin(), astronauts.end(), [](pii& a, pii& b) {
                return a.second > b.second;
            });

            pads.push_back({building_id, pos, astronauts});
        } else {
            buildings.push_back({building_id, pos, type});
        }
    }

    sol = Solution(resources, tubes, teleporters, pods, sol.buildings, sol.pads);
    sol.add_buildings(buildings, pads);
}


int main() {
    Solution sol;

    while (1) {
        cerr << "new loop, before parsing input\n";
        parse_input(sol);

        vector<Move> moves = sol.greedy_teleporters();

        for(Move& move : moves) {
            cout << "TELEPORT " << move.args[0] << " " << move.args[1] << ';';
        }
        if(moves.empty()) {
            cout << "WAIT;";
        }
        cout << endl;
    }
}

