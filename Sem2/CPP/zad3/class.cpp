#include "class.hpp"


Number::Number() : Number(0) {}
Number::Number(double n){
    nums_tab = new double[max_len];
    nums_tab[0] = n;
    tab_ptr = 0;
    elements = 1;
}

Number::Number(Number& n){      // assigment constructor
    insert(n.get_num());
}

Number::Number(Number&& n){     // move constructor
    elements = n.elements;
    tab_ptr = n.tab_ptr;
    nums_tab = n.nums_tab;
}

Number::~Number(){
    delete[] nums_tab;
}

void Number::insert(double n){
    tab_ptr++;
    if(tab_ptr == INT_MAX){
        tab_ptr %= max_len;
        tab_ptr += 2*max_len;
    }
    nums_tab[tab_ptr%max_len] = n;
}

const double Number::get_num(){
    if(tab_ptr < 0) return 0;
    return nums_tab[tab_ptr%max_len];
}

const double Number::get_history(int steps_back){
    if(steps_back == 0 || steps_back >= max_len || tab_ptr - steps_back < 0) return 0;
    return nums_tab[tab_ptr-steps_back];
}

void Number::revert(int steps_back){
    if(steps_back == 0 || steps_back >= max_len || tab_ptr - steps_back < 0) return;
    insert(nums_tab[tab_ptr-steps_back]);
}

Number& Number::operator=(Number n){
    insert(n.get_num());
    return *this;
}

Number& Number::operator=(Number&& n){
    elements = n.elements;
    tab_ptr = n.tab_ptr;
    nums_tab = n.nums_tab;
    return *this;
}
