#include <bits/stdc++.h>
using namespace std;

#define cerr if(0) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;
typedef pair<int, ll> pill;
typedef pair<ll, int> plli;
typedef pair<ll, ll> pll;


constexpr int L = 1e6+4;
constexpr int MOD = 1e9+7;
constexpr int CHARS_N = 'z'-'a'+1;


class Trie {
    struct Node {
        int children[CHARS_N];
        bool word_ends = false;
    };

public:
    const int root = 0;
    vector<Node> nodes;

    Trie() {
        nodes.emplace_back(Node());
    }

    void printTree(int node, bool isRoot = false, string prefix = "") const {
        if (!isRoot && !nodes[node].word_ends) {
            bool hasChild = false;
            for (int i = 0; i < CHARS_N; ++i) {
                if (nodes[node].children[i]) {
                    hasChild = true;
                    break;
                }
            }
            if (!hasChild) return;
        }

        if (!isRoot) {
            cerr << prefix << (nodes[node].word_ends ? "*" : "") << '\n';
        }

        for (int i = 0; i < CHARS_N; ++i) {
            int child = nodes[node].children[i];
            if (child) {
                char c = 'a' + i;
                printTree(child, false, prefix + c);
            }
        }
    }

    inline int nextNode(const int node, const char child) const {
        return nodes[node].children[child-'a'];
    }
    inline int nextNode(const int node, const int child) const {
        return nodes[node].children[child];
    }

    void addNode(const int node, const int child) {
        const int idx = nodes.size();
        nodes.emplace_back(Node());
        nodes[node].children[child] = idx;
    }

    void addWord(const string &s) {
        int curr = root;

        for (const char c : s) {
            int i = c-'a';

            if (!nextNode(curr, i)) {
                addNode(curr, i);
            }

            curr = nextNode(curr, i);
        }

        nodes[curr].word_ends = true;
    }

    int findNextWord(int node, const string &s, int &ptr) const {
        if (!nextNode(node, s[ptr]))
            return 0;
        node = nextNode(node, s[ptr]);
        ptr++;
        
        while (!nodes[node].word_ends && ptr < s.size()) {
            if (!nextNode(node, s[ptr]))
                return 0;
            node = nextNode(node, s[ptr]);
            
            ptr++;
        }

        return node;
    }
};


int cnt_splits(string &s, Trie &trie) {
    vector<int> cnt(s.size() + 1, 0);
    cnt[s.size()] = 1;

    for (int i = s.size() - 1; i >= 0; i--) {
        int node = trie.root;
        int ptr = i;

        while (ptr < s.size()) {
            int prev_ptr = ptr;
            int next_node = trie.findNextWord(node, s, ptr);

            if (!next_node) break;
            
            if (trie.nodes[next_node].word_ends) {
                cnt[i] = (cnt[i] + cnt[ptr]) % MOD;
            }

            node = next_node;
            
            if (ptr == prev_ptr) break;
        }
    }

    return cnt[0];
}


int main() {
    // ios_base::sync_with_stdio(0);
    // cin.tie(0);

    string s; cin >> s;
    int k; cin >> k;

    Trie trie;

    for (int i = 0; i < k; i++) {
        string w; cin >> w;
        trie.addWord(w);
    }

    trie.printTree(trie.root, true);
    cerr << "------------------\n";

    cout << cnt_splits(s, trie) << '\n';
}

