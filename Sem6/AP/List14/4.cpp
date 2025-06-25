#include <bits/stdc++.h>
using namespace std;

#define cerr if(1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;
typedef pair<int, ll> pill;
typedef pair<ll, int> plli;
typedef pair<ll, ll> pll;


/*
we have been given number k, and we want to find such pairs of numbers (without duplicates), such that xor >= k
assume we found one number a - now we want to find how many are there numbers b, fitting the condition
observation - we can calculate that deterministically, as for the number X to be greater than Y, it has to have:
- if so far (counting from MSB) X===Y and 
    - Y is now 1, then X has to be 1
    - Y is 0 then X can be 0 or 1

using trie, storing numbers from MSB at the root, and storing number of words in subtree  
we can calcluate a xor k and compare that number with all numbers we search thru
*/



constexpr int L = 1e6+4;
constexpr int MOD = 1e9+7;
constexpr int ALPHABET_SIZE = 2;
constexpr int ALPHABET_CHAR_START = '0';
constexpr int MAX_BIT = 20;


class Trie {
    struct Node {
        int children[ALPHABET_SIZE] = {0};
        bool word_ends = false;
        int subtree_size = 0;
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
            for (int i = 0; i < ALPHABET_SIZE; ++i) {
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

        for (int i = 0; i < ALPHABET_SIZE; ++i) {
            int child = nodes[node].children[i];
            if (child) {
                char c = ALPHABET_CHAR_START + i;
                printTree(child, false, prefix + c);
            }
        }
    }

    inline int nextNode(const int node, const char child) const {
        return nodes[node].children[child-ALPHABET_CHAR_START];
    }
    inline int nextNode(const int node, const int child) const {
        return nodes[node].children[child];
    }

    void addNode(const int node, const int child) {
        const int idx = nodes.size();
        nodes.emplace_back(Node());
        nodes[node].children[child] = idx;
    }

    void addWord(const int s) {
        int curr = root;

        for (int bit = MAX_BIT; bit >= 0; --bit) {
            int i = (s >> bit) & 1;

            if (!nextNode(curr, i)) {
                addNode(curr, i);
            }

            nodes[curr].subtree_size++;
            curr = nextNode(curr, i);
        }

        nodes[curr].word_ends = true;
        nodes[curr].subtree_size++;
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



int cnt_greater_numbers(int a, int k, const Trie &trie) {
    int node = trie.root;
    int cnt = 0;

    for (int bit = MAX_BIT; bit >= 0; --bit) {
        int abit = (a >> bit) & 1;
        int kbit = (k >> bit) & 1;

        for (int bbit = 0; bbit < 2; ++bbit) {
            int xorbit = abit ^ bbit;

            if (xorbit > kbit) {
                if (trie.nextNode(node, bbit)) {
                    cnt += trie.nodes[trie.nextNode(node, bbit)].subtree_size;
                }
            }
        }

        int bbit = abit ^ kbit;
        if (!trie.nextNode(node, bbit)) break;

        node = trie.nextNode(node, bbit);
    }

    return cnt;
}




int main() {
    // ios_base::sync_with_stdio(0);
    // cin.tie(0);

    int n; cin >> n;
    int k; cin >> k;

    
    vector<int> numbers(n);

    for (int i = 0; i < n; i++) {
        int w; cin >> w;
        numbers[i] = w;
    }


    int res = 0;
    Trie trie;

    for (int i = 0; i < n; ++i) {
        res += cnt_greater_numbers(numbers[i], k, trie);
        cerr << "Count for " << numbers[i] << ": " << res << '\n';

        trie.addWord(numbers[i]);
    }

    cout << res << '\n';
}

