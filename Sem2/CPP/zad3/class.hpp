#include <bits/stdc++.h>
using namespace std;


class Number{
private:
    const int max_len = 3;
    int elements, tab_ptr;
    double *nums_tab;
public:
    Number();
    Number(double n);
    Number(Number& n);
    Number(Number&& n);
    ~Number();
    
    void insert(double n);
    const double get_num();
    const double get_history(int steps_back);
    void revert(int steps_back);
    Number& operator=(Number n);
    Number& operator=(Number&& n);
};
