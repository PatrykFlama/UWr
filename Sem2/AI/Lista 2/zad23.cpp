#include <bits/stdc++.h>
using namespace std;
#define pii pair<int, int>


bool board[30][30];             // board[y][x]
set<pair<pii, set<pii>>> vis;   // pair sokoban positions and boxes positions
int y_size;
int x_size;
string directions = "LRUD";

int goal_distance[30][30];
set<pii> goals;


struct State {
    int x, y;
    set<pii> positions;     // positions of boxes
    string moves;
};

bool operator <(const State& x, const State& y) { // used to compare two states when inserting to set
    return x.positions != y.positions and x.moves > y.moves;
}

State get_input(){
    /*
    W - wall
    K - sokoban
    B - box
    G - goal
    * - box on goal
    */
    ifstream myfile("zad_input.txt");
    vector<string> v;
    string line;
    while(getline(myfile, line)){
        v.push_back(line);
    }

    int x, y;
    set<pii> initial_positions; 

    for(int i = 0; i < v.size(); i++){
        for(int j = 0; j < v[i].size(); j++){ 
            if(v[i][j] == 'G' or v[i][j] == '*' or v[i][j] == '+'){
                goals.insert({i, j});
            }
            if(v[i][j] == 'K' or v[i][j] == '+'){
                y = i;
                x = j;
            }
            if(v[i][j] == 'B' or v[i][j] == '*'){
                initial_positions.insert({i, j});
            }

            if(v[i][j] == 'W'){
                board[i][j] = 1;
            }
            else{
                board[i][j] = 0;
            }
        }
    }

    y_size = v.size();
    x_size = v[0].size();

    State initial_state;
    initial_state.x = x; initial_state.y = y;
    initial_state.positions = initial_positions;
    initial_state.moves = "";
    return initial_state;
}

void print_result(string a){
    ofstream myfile;
    myfile.open("zad_output.txt");

    myfile << a + "\n";

    myfile.close();
}

bool position_safe(pii position){   // check if position is in the map borders
    int y = position.first;
    int x = position.second;
    return x >= 0 and y >= 0 and y < y_size and x < x_size;
}

bool position_not_occupied(pii position){       // check if position is free
    int y = position.first;
    int x = position.second;
    return position_safe(position) and board[y][x] == 0;
} 

pii position_move(pii position, int x){     // move position in given direction - //? left, right, up, down
    if(x == 0) return {position.first, position.second - 1};
    if(x == 1) return {position.first, position.second + 1};
    if(x == 2) return {position.first - 1, position.second};
    if(x == 3) return {position.first + 1, position.second};
}

set<State> get_avaible_moves(pii sokoban_pos, set<pii> pos){
    set<State> result;

    queue<pair<pii, string>> q;                 // queue consists of positions
    bool vis[30][30];                           // and moves that lead to this position
    for(int i = 0; i < y_size; i++){            // from initial sokoban position
        for(int j = 0; j < x_size; j++){
            vis[i][j] = false;
        }
    }

    q.push({sokoban_pos, ""});
    vis[sokoban_pos.first][sokoban_pos.second] = true;

    while(!q.empty()){
        pair<pii,string> top = q.front();
        q.pop();
        pii position = top.first;
        string moves = top.second;

        for(int i = 0; i <= 3; i++){
            pii new_position = position_move(position, i);
            // if it is on map, and it's no wall and it's no box and not visited then go there and add to queue
            if(!pos.count(new_position) and
               position_not_occupied(new_position) and
               !vis[new_position.first][new_position.second])
            {
                q.push({new_position, moves + directions[i]});
                vis[new_position.first][new_position.second] = true;
            }   

            if(pos.count(new_position)){  //we check whether this is new state - we can move one of the boxes
                // move chest case (when we want to stay on place occupied by chest)
                pii new_box_position = position_move(new_position, i);
                if(!pos.count(new_box_position) and position_not_occupied(new_box_position)){
                    State new_state;
                    set<pii> new_positions;
                    for(auto pos : pos){
                        if(pos == new_position){ //original box position
                            new_positions.insert(new_box_position); //new box position
                            continue;
                        }
                        new_positions.insert(pos); //we need to add other boxes positions which doesn't move
                    }
                    
                    new_state.positions = new_positions;
                    new_state.y = new_position.first; new_state.x = new_position.second;
                    new_state.moves = moves + directions[i];
                    result.insert(new_state);
                }
            }
        }
    }

    return result;
}

/* #region //* moving funcitons */
pii move_left(pii position){
    pii res = position;
    if(position_not_occupied({position.first, position.second - 1})){
        res = {position.first, position.second - 1};
    }
    return res;
}
pii move_right(pii position){
    pii res = position;
    if(position_not_occupied({position.first, position.second + 1})){
        res = {position.first, position.second + 1};
    }
    return res;
}
pii move_up(pii position){
    pii res = position;
    if(position_not_occupied({position.first - 1, position.second})){
        res = {position.first - 1, position.second};
    }
    return res;
}
pii move_down(pii position){
    pii res = position;
    if(position_not_occupied({position.first + 1, position.second})){
        res = {position.first + 1, position.second};
    }
    return res;
}
/* #endregion */

void change_nearest_goals(pii init){
    bool vis[30][30];
    queue<pair<pii, int>> q;
    q.push({init, 0});
    vis[init.first][init.second] = true;
    for(int i=0; i<y_size; i++){
        for(int j=0; j<x_size; j++){
            vis[i][j] = false;
        }
    }

    while(!q.empty()){
        pair<pii,int> top = q.front();
        q.pop();
        pii position = top.first;
        int distance = top.second;

        goal_distance[position.first][position.second] = min(goal_distance[position.first][position.second], distance);

        pii new_position;
        
        new_position = move_left(position);
        if(!vis[new_position.first][new_position.second]){
            q.push({new_position, distance + 1});
            vis[new_position.first][new_position.second] = true;
        }
        new_position = move_right(position);
        if(!vis[new_position.first][new_position.second]){
            q.push({new_position, distance + 1});
            vis[new_position.first][new_position.second] = true;
        }
        new_position = move_up(position);
        if(!vis[new_position.first][new_position.second]){
            q.push({new_position, distance + 1});
            vis[new_position.first][new_position.second] = true;
        }
        new_position = move_down(position);
        if(!vis[new_position.first][new_position.second]){
            q.push({new_position, distance + 1});
            vis[new_position.first][new_position.second] = true;
        }
    }
}

void create_distance_array(){   // our heuristics array
    // it creates table of distances from all goals to all possible cells
    for(int i = 0; i < y_size; i++){
        for(int j = 0; j < x_size; j++){
            goal_distance[i][j] = INT_MAX;
        }
    }
    for(auto pos : goals){
        change_nearest_goals(pos);
    }
}

int dist(set<pii> positions){   // get max dist
    int res = INT_MIN;
    for(pii pos : positions){
        res = max(res, goal_distance[pos.first][pos.second]);
    }
    return res;
}


bool win(set<pii> positions){   // check if game is won
    for(auto pos : positions)
        if(!goals.count(pos))
            return false;

    return true;
}


int main(){
    State initial_state = get_input();

    priority_queue<pair<int, State>> pq;
    pq.push({0, initial_state});
    vis.insert({{initial_state.y, initial_state.x}, initial_state.positions});
    create_distance_array();


    while(pq.size()){        //* BFS with heuristics
        State top_state = pq.top().second;
        pq.pop();

        int x = top_state.x; int y = top_state.y;
        set<pii> positions =  top_state.positions;
        string moves = top_state.moves;


        if(win(positions)){
            print_result(moves);
            break;
        }

        // set of possible next state after moving one of the boxes
        set<State> avaible_moves = get_avaible_moves({y, x}, positions);
        
        // now we try to move all of the boxes
        for(State state : avaible_moves){          
            if(!vis.count({{state.y, state.x}, state.positions})){
                string new_moves = moves + state.moves;
                state.moves = new_moves;

                int heur = -(new_moves.size() + dist(state.positions));
                pq.push({heur, state});
                vis.insert({{state.y, state.x}, state.positions});
            }
        }
    }
}
