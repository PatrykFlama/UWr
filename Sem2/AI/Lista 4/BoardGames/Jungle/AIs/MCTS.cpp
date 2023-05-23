#pragma once
#include <bits/stdc++.h>
using namespace std;
#include "Jungle.cpp"
using Clock = std::chrono::high_resolution_clock;



class Node{
public:
    vector<int> children;   // todo - actually use it
    bool is_leaf = true;
    int times_sampled = 0;
    int avg_value = 0;
};

class MCTS{
    unordered_map<int, Node> tree;
    int main_player;
public:
    MCTS(){
        srand(time(NULL));
        tree[Jungle(0).hash] = Node();
        tree[Jungle(1).hash] = Node();
    }

    pair<int, pair<int, int>> gen_next_move(Jungle* state, int time){
        run_mcts_for(time, state);
        return get_best_move(state);
    }

    pair<int, pair<int, int>> get_best_move(Jungle* state){            // {piece, {dirx, diry}}
        vector<pair<int, pair<int, int>>> legal_moves = state->get_legal_moves();
        pair<int, pair<int, int>> best_move;
        int max_score = INT_MIN;

        for(auto [piece, dir] : legal_moves){
            int score = tree[state->gen_next_state(piece, dir).hash].avg_value;
            if(score > max_score){
                max_score = score;
                best_move = {piece, dir};
            }
        }

        return best_move;
    }

    void run_mcts_for(int milliseocnds, Jungle* state){
        main_player = state->player;
        auto start = Clock::now();
        while(chrono::duration_cast<chrono::milliseconds>(Clock::now() - start).count() < milliseocnds){
            mcts(state);
        }
    }

    void mcts(Jungle* state){
        // tree traversal phase:
        Node* here = &tree[state->hash];
        vector<Node*> path;
        pair<int, pair<int, int>> best_move;
        path.push_back(here);
        while(not here->is_leaf){       // get leaf node in mcts tree
            int max_ucb = INT_MIN;
            for(auto [piece, dir] : state->get_legal_moves()){
                int next_hash = state->gen_next_hash(piece, dir);
                int curr_ucb = ucb1(next_hash, here->times_sampled);
                if(curr_ucb > max_ucb){
                    best_move = {piece, dir};
                    max_ucb = curr_ucb;
                }
            }

            state->execute_move(best_move);
            here = &tree[state->hash];
            path.push_back(here);
        }


        int result;
        if(tree[state->hash].times_sampled == 0) result = rollout(state);
        else{   // node expansion
            Node* here = &tree[state->hash];
            here->is_leaf = false;
            vector<pair<int, pair<int, int>>> legal_moves = state->get_legal_moves();
            for(auto [piece, dir] : legal_moves){       // create new nodes for each possible move and save children
                int next_hash = state->gen_next_hash(piece, dir);
                here->children.push_back(next_hash);            //todo
                tree[next_hash] = Node();
            }
            if(legal_moves.size() == 0) result = rollout(state);      // do rollout for one of those new states
            else{
                Jungle next_state = state->gen_next_state(legal_moves[0].first, legal_moves[0].second);
                result = rollout(&next_state);
            }
        }

        // backpropagation phase:
        for(auto node : path){
            // node->avg_value += (result - node->avg_value)/node->times_sampled;
            node->avg_value += result;
            if(node->times_sampled != 0) node->avg_value /= 2;
            node->times_sampled++;
        }
    }

    int ucb1(int node_hash, int parent_visits){
        Node* here = &tree[node_hash];
        if(here->times_sampled == 0) return INT_MAX;
        return here->avg_value + 2*sqrt(log(parent_visits)/here->times_sampled);
    }

    int rollout(Jungle* state){
        Jungle temp_state = *state;
        vector<pair<int, pair<int, int>>> legal_moves = temp_state.get_legal_moves();
        while(not temp_state.terminal(legal_moves)){
            // pick random move
            auto random = rand() % legal_moves.size();
            temp_state.execute_move(legal_moves[random].first, legal_moves[random].second);

            legal_moves = temp_state.get_legal_moves();
        }
        return temp_state.result(main_player);
    }
};