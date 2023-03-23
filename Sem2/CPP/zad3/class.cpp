#include "class.hpp"


Number::Number() : Number(0) {}
Number::Number(double n){
    nums_tab = new double[max_len];
    nums_tab[0] = n;
    tab_ptr = 0;
    elements = 1;
}


Number::Number(Number& other){      // assigment constructor
    nums_tab = new double[max_len];
    nums_tab[0] = other.get_num();
    tab_ptr = 0;
    elements = 1;
}

Number& Number::operator=(Number& other){
    insert(other.get_num());
    return *this;
}


Number::Number(Number&& other){      // move constructor
    elements = other.elements;
    tab_ptr = other.tab_ptr;
    nums_tab = other.nums_tab;
    other.nums_tab = 0x0;
}

Number& Number::operator=(Number&& other){
    elements = other.elements;
    tab_ptr = other.tab_ptr;
    nums_tab = other.nums_tab;
    other.nums_tab = 0x0;
    return *this;
}


Number::~Number(){
    if(nums_tab != 0x0) delete[] nums_tab;
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
    int pos = abs(tab_ptr % max_len);
    return nums_tab[pos];
}

const double Number::get_history(int steps_back){
    // if(steps_back >= max_len) return 0;             // returns 0 if history is out of range
    if(steps_back >= max_len) throw invalid_argument( "history out of range!" );
    int pos = abs((tab_ptr - steps_back) % max_len);
    return nums_tab[pos];
}

void Number::revert(int steps_back){
    // if(steps_back == 0 || steps_back >= max_len) return;
    if(steps_back == 0) return;
    if(steps_back >= max_len) throw invalid_argument( "history out of range!" );
    int pos = abs((tab_ptr - steps_back) % max_len);
    insert(nums_tab[pos]);
}

ostream& operator<<(ostream& str, Number& number){
    return str << number.get_num();
}
