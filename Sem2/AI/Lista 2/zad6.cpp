#include <bits/stdc++.h>
using namespace std;
#define pii pair<int, int>
const double epsillon = 2;


struct State{
    string moves;
    set<pii> positions;
};

bool operator<(const State& x, const State& y) {
    return x.moves.size() > y.moves.size();
}

bool board[30][30];
int goal_distance[30][30];
int y_size;
int x_size;
set<pii> starting_positions;
set<pii> goal_positions;


void read_input(){
    ifstream file("zad_input.txt");
    string line;

    int row = 0, col;
    while(getline(file, line)){
        for(col = 0; col < line.size(); col++){
            switch (line[col]){
            case 'B':
                starting_positions.insert({row, col});
                goal_positions.insert({row, col});
                break;
            case 'G':
                goal_positions.insert({row, col});
                break;
            case 'S':
                starting_positions.insert({row, col});
                break;
            case '#':
                board[row][col] = 1;
                continue;
            }

            board[row][col] = 0;
        }
        row++;
    }

    y_size = row;
    x_size = col;
    file.close();
}

void print_result(string a){
    ofstream file;
    file.open("zad_output.txt");
    file << a + "\n";
    file.close();
}

bool safe(pii position){
    int y = position.first;
    int x = position.second;
    return x >= 0 && x < x_size && y >= 0 && y < y_size;
}

bool available(pii position){
    int y = position.first;
    int x = position.second;
    return safe(position) and board[y][x] == 0;
}   

/* #region //* move functions */
string directions = "LRUD";
const int dirs[4][2] = {{0, -1}, {0, 1}, {-1, 0}, {1, 0}};
#define LEFT 0
#define RIGHT 1
#define UP 2
#define DOWN 3

pii move(pii pos, int dir){
    pii res = {pos.first + dirs[dir][0], pos.second + dirs[dir][1]};
    if(available(res)) return res;
    else return pos;
}

set<pii> make_move(set<pii> positions, int dir){
    set<pii> res;

    for(auto pos : positions){
        pii new_pos = {pos.first + dirs[dir][0], pos.second + dirs[dir][1]};
        if(available(new_pos)) res.insert(new_pos);
        else res.insert(pos);
    }

    return res;
}
/* #endregion */


void calc_dist_from_goals(pii start){   // BFS
    bool vis[30][30];
    queue<pair<pii, int>> q;    // queue of {pos and dist}
    q.push({start, 0});

    for(int i = 0; i < y_size; i++)
        for(int j = 0; j < x_size; j++)
            vis[i][j] = false;
    vis[start.first][start.second] = true;

    while(!q.empty()){
        auto [position, distance] = q.front();
        q.pop();

        goal_distance[position.first][position.second] = min(goal_distance[position.first][position.second], distance);

        // move in every possible direction
        for(int dir = 0; dir < 4; dir++){
            pii new_position = move(position, dir);
            if(!vis[new_position.first][new_position.second]){
                q.push({new_position, distance + 1});
                vis[new_position.first][new_position.second] = true;
            }
        }
    }
}

void calc_distance_array(){ // create heuristics dist table, which contains closest distances to goal
    for(int i = 0; i < y_size; i++)
        for(int j = 0; j < x_size; j++)
            goal_distance[i][j] = INT_MAX;

    for(auto pos : goal_positions)
        calc_dist_from_goals(pos);
}

int dist(set<pii> positions){   // worst case distance from the goal (by heuristics)
    int res = -1;
    for(auto pos : positions)
        res = max(res, goal_distance[pos.first][pos.second]);
    return res*(1+epsillon);        //! here we make heuristics inadmissible
}

bool win(set<pii> positions){
    for(auto pos : positions)
        if(!goal_positions.count(pos))
            return false;
    return true;
}


int main(){
    read_input();
    calc_distance_array();
    
    State starting_state;
    starting_state.positions = starting_positions;
    starting_state.moves = "";

    // pq is sorted by heuristics distances + steps already taken
    priority_queue<pair<int, State>> pq;
    pq.push({(-1)* dist(starting_state.positions), starting_state});

    set<set<pii>> vis;
    vis.insert(starting_state.positions);

    while(pq.size()){
        pair<int, State> top = pq.top();
        pq.pop();
        auto [moves, positions] = top.second;

        if(win(positions)){
            print_result(moves);
            return 0;
        }

        set<pii> new_positions;
        State s;
        
        for(int i = 0; i < 4; i++){
            new_positions = make_move(positions, i);
            s.positions = new_positions;
            s.moves = moves + directions[i];

            if(vis.find(new_positions) == vis.end()){  
                pq.push({(-1)*(dist(new_positions) + moves.size()), s});
                vis.insert(new_positions);
            }
        } 
    }
}
