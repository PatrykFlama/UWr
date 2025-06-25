#include <bits/stdc++.h>
using namespace std;

#define cerr if (1) cout
#define fst first
#define snd second
typedef long long ll;
typedef pair<int, int> pii;
typedef pair<int, ll> pill;
typedef pair<ll, int> plli;
typedef pair<ll, ll> pll;

constexpr int L = 1e6 + 4;
constexpr int MOD = 1e9 + 7;
constexpr int CHARS_N = 'z' - 'a' + 1;


class Trie {
public:
    struct Node {
        int children[CHARS_N] = {0};
        int fail = 0;
        int output;
    };

    vector<Node> nodes;

    Trie() {
        nodes.emplace_back();
    }


    void addWord(const string &s, int index) {
        int curr = 0;
        for (char c : s) {
            int i = c - 'a';
            if (!nodes[curr].children[i]) {
                nodes[curr].children[i] = nodes.size();
                nodes.emplace_back();
            }
            curr = nodes[curr].children[i];
        }
        // nodes[curr].output.push_back(index);
        nodes[curr].output = index;
    }

    void build() {
        queue<int> q;
        for (int i = 0; i < CHARS_N; ++i) {
            int child = nodes[0].children[i];
            if (child) {
                nodes[child].fail = 0;
                q.push(child);
            }
        }

        while (q.size()) {
            int node = q.front();
            q.pop();

            for (int i = 0; i < CHARS_N; ++i) {
                int child = nodes[node].children[i];
                if (!child) continue;

                int f = nodes[node].fail;
                while (f && !nodes[f].children[i]) {
                    f = nodes[f].fail;
                }

                if (nodes[f].children[i]) {
                    f = nodes[f].children[i];
                }

                nodes[child].fail = f;

                q.push(child);
            }
        }
    }

    vector<int> search(const string &t, int pattern_count) {
        vector<int> result(pattern_count);
        int node = 0;

        for (char c : t) {
            int i = c - 'a';

            while (node && !nodes[node].children[i]) {
                node = nodes[node].fail;
            }

            if (nodes[node].children[i]) {
                node = nodes[node].children[i];
            }

            for (int v = node; v; v = nodes[v].fail) {
                result[nodes[v].output]++;
            }
        }

        return result;
    }
};


int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    string t;
    int k;
    cin >> t >> k;

    vector<string> patterns(k);
    Trie trie;

    for (int i = 0; i < k; ++i) {
        cin >> patterns[i];
        trie.addWord(patterns[i], i);
    }

    trie.build();
    vector<int> result = trie.search(t, k);

    for (int count : result) {
        cout << count << '\n';
    }
}
