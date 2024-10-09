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
// ----- BASIC STRUCTS -----
class Point {
public:
    int x;
    int y;

    Point() {}
    Point(int x, int y) : x(x), y(y) {}

    double dist(const Point& p) const {
        return sqrt(pow((p.x - x), 2) + pow((p.y - y), 2));
    }

    friend ostream& operator<<(ostream& os, const Point& p) {
        os << p.x << " " << p.y;
        return os;
    }
};

class Segment {
    const double epsilon = 0.0000001;
public:
    Point p1;
    Point p2;

    Segment() {}
    Segment(Point p1, Point p2) : p1(p1), p2(p2) {}

    double length() const {
        return p1.dist(p2);
    }

    bool is_point_on_segment(const Point& C) const {
        return abs(p1.dist(C) + p2.dist(C) - p1.dist(p2)) < epsilon;
    }

    int orientation(const Point& C) const {
        int prod = (C.y - p1.y) * (p2.x - p1.x) - (p2.y - p1.y) * (C.x - p1.x);
        return (prod > 0) - (prod < 0); // returns 1, 0, or -1
    }

    bool intersects_segment(const Segment& s) const {
        const Point& C = s.p1, D = s.p2;
        return this->orientation(C) * this->orientation(D) < 0 && s.orientation(p1) * s.orientation(p2) < 0;
    }

    friend ostream& operator<<(ostream& os, const Segment& s) {
        os << s.p1 << " " << s.p2;
        return os;
    }
};

// ----- GAME STRUCTS -----

class Building {
public:
    int id;
    Point pos;

    Building() {}
    Building(int id, Point pos) : id(id), pos(pos) {}
};

class LandingPad : public Building {
public:
    vector<pii> astronauts; // {type, amt}  sorted by amt

    LandingPad() {}
    LandingPad(int id, Point pos, vector<pii> astronauts) : Building(id, pos), astronauts(astronauts) {}
};

class Destination : public Building {
public:
    int type;   

    Destination() {}
    Destination(int id, Point pos, int type) : Building(id, pos), type(type) {}
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
    Tube(int id, int building1, int building2, int capacity=1) : 
        id(id), building1(building1), building2(building2), capacity(capacity) {}
};

class Teleporter {
public:
    int id;
    int building_from;
    int building_to;

    Teleporter() {}
    Teleporter(int id, int building_from, int building_to) : 
            id(id), building_from(building_from), building_to(building_to) {}
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
    unordered_map<int, Destination> buildings;
    unordered_map<int, LandingPad> pads;

    // unordered_map<int, vector<pii>> graph;  // {fromBuildingID, [{toBuildingID, cost}]}
    // vector<vector<int>> dist;  // dist[from][to] = cost

    Solution() {}
    Solution(int resources, vector<Tube> &tubes, vector<Teleporter> &teleporters, vector<Pod> &pods, vector<Destination> &buildings, vector<LandingPad> &pads) {
        recreate_info(resources, tubes, teleporters, pods);
        add_buildings(buildings, pads);
    }

    void recreate_info(int resources, vector<Tube> &tubes, vector<Teleporter> &teleporters, vector<Pod> &pods) {
        this->resources = resources;

        for (Tube& tube : tubes) {
        }
        (this->tubes).clear();
        for(Tube& tube : tubes) {
            this->tubes[tube.id] = tube;
        }
        (this->teleporters).clear();
        for(Teleporter& teleporter : teleporters) {
            this->teleporters[teleporter.id] = teleporter;
        }
        (this->pods).clear();
        for(Pod& pod : pods) {
            this->pods[pod.id] = pod;
        }
    }

    void add_buildings(vector<Destination> &buildings, vector<LandingPad> &pads) {
        for(Destination& building : buildings) {
            this->buildings[building.id] = building;
        }
        for(LandingPad& pad : pads) {
            this->pads[pad.id] = pad;
        }
    }

    friend ostream& operator<<(ostream& os, const Solution& sol) {
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
        for(const auto& [id, building] : sol.buildings) {
            os << building.id << " " << building.pos.x << " " << building.pos.y << " " << building.type << '\n';
        }
        os << "Landing pads: \n";
        for(const auto& [id, pad] : sol.pads) {
            os << pad.id << " " << pad.pos.x << " " << pad.pos.y << ": ";
            for(const pii& astronaut : pad.astronauts) {
                os << astronaut.first << " " << astronaut.second << "";
            }
            os << "\n";
        }

        return os;
    }


    vector<Move> greedy_teleporters() {
        //TODO prefer buildings with the least number of ways to get to
        // sort pads by the number of astronauts
        vector<int> pads_order;
        for(auto& [id, pad] : pads) {
            // check if pad already has teleporter
            bool has_teleporter = false;
            for(auto& teleporter : teleporters) {
                if(teleporter.second.building_from == pad.id) {
                    has_teleporter = true;
                    break;
                }
            }
            if(has_teleporter) continue;

            pads_order.push_back(id);
        }

        sort(pads_order.begin(), pads_order.end(), [&](int a, int b) {
            return pads[a].astronauts[0].second > pads[b].astronauts[0].second;
        });

        vector<Move> moves;
        for(int id : pads_order) {
            LandingPad& pad = pads[id];
            if(resources < COSTS[TELEPORT]) break;

            for(pii& astronaut : pad.astronauts) {
                bool has_building = false;
                for(auto& [id, building] : buildings) {
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

                    //! TODO KINDA STINKY TRICK
                    astronaut.second = INT_MIN;
                    sort(pad.astronauts.begin(), pad.astronauts.end(), [](pii& a, pii& b) {
                        return a.second > b.second;
                    });

                    break;
                }
                if(has_building) break;
            }
        }

        return moves;
    }

    vector<Move> greedy_tubes() {
        vector<Move> moves;
        for(auto& [id, pad] : pads) {
            // check if pad has > 4 tubes
            {
                int occupancy = 0;
                for(auto& tube : tubes) {
                    if(tube.second.building1 == pad.id || tube.second.building2 == pad.id) {
                        occupancy++;
                    }
                }
                if(occupancy > 4) continue;
            }

            for(pii& astronaut : pad.astronauts) {
                if(resources < COSTS[POD]) break;

                // check if pad already has connection to astronaut's destination
                bool has_connection = false;
                for(auto& [id, tube] : tubes) {
                    if((tube.building1 == pad.id && buildings[tube.building2].type == astronaut.first) ||
                        (tube.building2 == pad.id && buildings[tube.building1].type == astronaut.first)) {
                        has_connection = true;
                        break;
                    }
                }
                if(has_connection) continue;
                for(auto& [id, teleporter] : teleporters) {
                    if(teleporter.building_from == pad.id && buildings[teleporter.building_to].type == astronaut.first) {
                        has_connection = true;
                        break;
                    }
                }
                if(has_connection) continue;


                // find closest building of astronaut's type
                int closest_building_id = -1;
                for(auto& [id, building] : buildings) {
                    if(building.type != astronaut.first) 
                        continue;

                    // check if building is occupied <=> has more than 4 tubes
                    int occupation = 0;
                    for(auto& tube : tubes) {
                        if(tube.second.building2 == building.id || tube.second.building1 == building.id) {
                            // tube already exists
                            if(tube.second.building1 == pad.id || tube.second.building2 == pad.id) {
                                occupation = 5;
                            }
                            occupation++;
                        }
                    }
                    if(occupation > 4) continue;

                    // check if building is closer than the current closest
                    if(closest_building_id != -1 && 
                    pad.pos.dist(building.pos) > pad.pos.dist(buildings[closest_building_id].pos))
                        continue;

                    // check if tube collides with some other tube
                    bool collides = false;
                    Segment new_tube_segment = {pad.pos, building.pos};
                    for(auto& [id, tube] : tubes) {
                        // if tube ends in same place (only one side) then it wont collide
                        if(tube.building1 == pad.id || tube.building2 == building.id)
                            continue;

                        //! TODO very not nice: assumes that pad is first building in any tube
                        Segment tube_segment = {pads[tube.building1].pos, buildings[tube.building2].pos};


                        if(tube_segment.intersects_segment(new_tube_segment)) {
                            collides = true;
                            break;
                        }
                    }
                    if(collides) continue;

                    // check if tube collides with some other building
                    collides = false;
                    for(auto& [id, building2] : buildings) {
                        if(building2.id == building.id) continue;

                        Segment new_tube_segment = {pad.pos, building.pos};


                        if(new_tube_segment.is_point_on_segment(building2.pos)) {
                            collides = true;
                            break;
                        }
                    }
                    if(collides) continue;
                    for(auto& [id, pad2] : pads) {
                        if(pad2.id == pad.id) continue;

                        Segment new_tube_segment = {pad.pos, building.pos};

                        if(new_tube_segment.is_point_on_segment(pad2.pos)) {
                            collides = true;
                            break;
                        }
                    }
                    if(collides) continue;

                    closest_building_id = building.id;
                }


                if(closest_building_id != -1) {
                    int total_cost = COSTS[TUBE]*pad.pos.dist(buildings[closest_building_id].pos) + COSTS[POD];
                    if(total_cost > resources) break;
                    resources -= total_cost;
                    
                    moves.push_back({TUBE, {pad.id, closest_building_id}});
                    tubes[tubes.size()*10] = {tubes.size()*10, pad.id, closest_building_id};

                    if(resources < COSTS[POD]) break;
                    moves.push_back({POD, {int(pods.size()), pad.id, closest_building_id}});
                    pods[int(pods.size())] = {int(pods.size()), {pad.id, closest_building_id, pad.id}};
                }
            }
        }

        return moves;
    }

    vector<Move> refill_pods() {
        // since (somehow) resources are not calculated accurately
        vector<Move> moves;

        for(auto& [id, tube] : tubes) {
            if(resources < COSTS[POD]) break;

            // ensure that pod does not exists already
            bool pod_exists = false;
            for(auto& [id, pod] : pods) {
                if(pod.stops[0] == tube.building1 && pod.stops[1] == tube.building2) {
                    pod_exists = true;
                    break;
                }
            }
            if(pod_exists) continue;

            resources -= COSTS[POD];
            moves.push_back({POD, {int(pods.size()), tube.building1, tube.building2}});
            pods[int(pods.size())] = {int(pods.size()), {tube.building1, tube.building2, tube.building1}};
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
        Point pos;
        cin >> type >> building_id >> pos.x >> pos.y;
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

    sol.recreate_info(resources, tubes, teleporters, pods);
    sol.add_buildings(buildings, pads);
}


int main() {
    Solution sol;

    while (1) {
        parse_input(sol);

        vector<Move> moves;

        // greedy refill
        moves = sol.refill_pods();
        for(Move& move : moves) {
            cout << MOVE_NAMES[move.type] << " ";
            for(int arg : move.args) {
                cout << arg << " ";
            }
            cout << ";";
        }

        // greedy tubes
        moves = sol.greedy_tubes();
        for(Move& move : moves) {
            cout << MOVE_NAMES[move.type] << " ";
            for(int arg : move.args) {
                cout << arg << " ";
            }
            cout << ";";
        }
        
        // greedy teleporters
        moves = sol.greedy_teleporters();
        for(Move& move : moves) {
            cout << MOVE_NAMES[move.type] << " ";
            cout << move.args[0] << " " << move.args[1] << ';';
        }

        cout << "WAIT" << endl;
    }
}

