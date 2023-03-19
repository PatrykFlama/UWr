#include <bits/stdc++.h>
using namespace std;

// TODO make .hpp header file

class Number{
private:
    const int max_len = 3;
    int elements, tab_ptr;
    double *nums_tab;
public:
    Number() : Number(0) {}
    Number(double n){
        nums_tab = new double[max_len];
        nums_tab[0] = n;
        tab_ptr = 0;
        elements = 1;
    }

    Number(Number& n){      // assigment constructor
        insert(n.get_num());
    }

    Number(Number&& n){     // move constructor
        elements = n.elements;
        tab_ptr = n.tab_ptr;
        nums_tab = n.nums_tab;
    }

    ~Number(){
        delete[] nums_tab;
    }

    void insert(double n){
        tab_ptr++;
        if(tab_ptr == INT_MAX){
            tab_ptr %= max_len;
            tab_ptr += 2*max_len;
        }
        nums_tab[tab_ptr%max_len] = n;
    }

    const double get_num(){
        if(tab_ptr < 0) return 0;
        return nums_tab[tab_ptr%max_len];
    }

    const double get_history(int steps_back){
        if(steps_back == 0 || steps_back >= max_len || tab_ptr - steps_back < 0) return 0;
        return nums_tab[tab_ptr-steps_back];
    }

    void revert(int steps_back){
        if(steps_back == 0 || steps_back >= max_len || tab_ptr - steps_back < 0) return;
        insert(nums_tab[tab_ptr-steps_back]);
    }

    Number& operator=(Number n){
        insert(n.get_num());
        return *this;
    }

    Number& operator=(Number&& n){
        elements = n.elements;
        tab_ptr = n.tab_ptr;
        nums_tab = n.nums_tab;
        return *this;
    }
};


int main(){

}