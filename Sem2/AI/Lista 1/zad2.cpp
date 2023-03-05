#include <bits/stdc++.h>
using namespace std;


int number_of_letters = 35;
char polish_letters[][3] = {"ą", "ę", "ć", "ł", "ń", "ó", "ś", "ź", "ż"};

int charnum(char c){
    if('a' <= c and c <= 'z')
        return int(c-'a');

    // polish letters: ą ę ć ł ń ó ś ź ż
    // for(int pol = 0; pol < sizeof(polish_letters); pol++)
    //     if(c == polish_letters[pol])
    //         return int('z'-'a'+pol+1);
}

struct Node{
    int next[26];
    bool is_end, is_leaf;

    Node(){
        memset(next, -1, sizeof(next));
        is_end = false, is_leaf = false;
    }
};

class Trie{
    vector<Node> nodes;
    int nodes_cnt = 1, words_cnt = 0, leaves_cnt = 0;

    void add_word(const string& s){
        int v = 0;
        for(char c : s){
            if(nodes[v].next[c-'a'] == -1){
                nodes[v].next[c-'a'] = nodes_cnt++;
                nodes[nodes[v].next[c-'a']].is_leaf = true;
                leaves_cnt++;

                if(nodes[v].is_leaf){
                    leaves_cnt--;
                    nodes[v].is_leaf = false;
                }
            }
            v = nodes[v].next[c-'a'];
        }

        if(!nodes[v].is_end)
            nodes[v].is_end = true, words_cnt++;
    }
};


int main(){
    string s;
    s += 'ą';
    cout << s.size() << "\n";
    return 0;
    cout << int("ą"[0]) << ' ' << int("ą"[1]) << ' ' << int("ą"[2]) << '\n';

    char c; cin >> c;
    cout << int(c) << '\n';
}
