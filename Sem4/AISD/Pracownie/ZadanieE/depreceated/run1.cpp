#include <bits/stdc++.h>
using namespace std;


template <class T>
class AVL {
    class Node {
    public:
        T val;
        Node *left, *right;
        int height;

        Node(T _val) : val(_val), left(nullptr), right(nullptr), height(1) {}
    };


    inline int height(Node *v) {
        return v ? v->height : 0;
    }

    inline int balance(Node *v) {
        return (v ? height(v->left) - height(v->right) : 0);
    }

    Node* rotate_right(Node *y) {    // returns new parent child
        Node *x = y->left;
        Node *beta = x->right;
        x->right = y;
        y->left = beta;
        y->height = max(height(y->left), height(y->right)) + 1;
        x->height = max(height(x->left), height(x->right)) + 1;
        return x;
    }

    Node* rotate_left(Node *x) {    // returns new parent child
        Node *y = x->right;
        Node *beta = y->left;
        y->left = x;
        x->right = beta;
        x->height = max(height(x->left), height(x->right)) + 1;
        y->height = max(height(y->left), height(y->right)) + 1;
        return y;
    }


    Node* _insert(Node* v, T val) {
        //* insert on leaf
        if(!v) return new Node(val);

        //* find proper place to insert
        if(val < v->val) 
            v->left = _insert(v->left, val);
        else if(v->val < val)
            v->right = _insert(v->right, val);
        else return v;  // if value exists do nothing
        
        //* update height
        v->height = max(height(v->left), height(v->right)) + 1;

        //* check for height difference > 1 => element did not exist before
        const int bal = balance(v);
        if(1 < bal) {   // higher on left tree
            if(val < v->left->val) {      // element added to left subtree of left tree => v->left is Node
                return rotate_right(v);
            } else {    // element in the middle
                v->left = rotate_left(v->left);
                return rotate_right(v);
            }
        } else if(bal < -1) {   // higher on right tree (mirrored case of left tree)
            if(v->right->val < val) {
                return rotate_left(v);
            } else {
                v->right = rotate_right(v->right);
                return rotate_left(v);
            }
        }

        return v;   // should never be returned from here
    }

    pair<Node*, bool> _remove(Node* v, T val) {     // TODO
    }

    Node *root;
    const T null_val;
public:
    AVL(T _null_val) : root(nullptr), null_val(_null_val) {}


    inline void insert(T val) {
        _insert(root, val);
    }

    inline bool remove(T val) {    //? returns false if val not found
        return _remove(root, val).second;
    }

    T lower(T val) {        //? returns null_val if lower bound of val not found
        Node* v = root;

        while(true) {
            if(v->right && v->right->val <= val)
                v = v->right;
            else if(v->left && v->left->val <= val)
                v = v->left;
            else break;
        }

        if(v->val > val) return null_val;
        return val;
    }

    T upper(T val) {        //? returns null_val if upper bound of val not found
        Node* v = root;

        while(true) {
            if(v->left && v->left->val >= val)
                v = v->left;
            else if(v->right && v->right->val >= val)
                v = v->right;
            else break;
        }

        if(v->val < val) return null_val;
        return val;
    }
};


int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(0);

    const long long null_val = LONG_LONG_MAX;
    AVL<long long> avl(null_val);

    int t; cin >> t;
    while(t--) {
        char op;
        int val;
        cin >> op >> val;

        switch (op) {
        case 'I':
            avl.insert(val);
            break;
        case 'D':
            if(avl.remove(val)) cout << "OK\n";
            else cout << "FAIL\n";
            break;
        case 'L':
        {
            const auto res = avl.lower(val);
            if(res == null_val) cout << "BRAK\n";
            else cout << res << '\n';
            break;
        }
        case 'U':
        {
            const auto res = avl.upper(val);
            if(res == null_val) cout << "BRAK\n";
            else cout << res << '\n';
            break;
        }
        default:
            break;
        }
    }
}