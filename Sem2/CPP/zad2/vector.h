#pragma once
#include <bits/stdc++.h>
using namespace std;

#ifndef DOUBLE_EPSILON
#define DOUBLE_EPSILON
const double double_epsilon = 0.000000000005;
#endif

class Vector{
    private:
    double x, y;
    public:
    Vector(double _x, double _y);
    Vector(const Vector& p);
    void change_x(double _x);
    void change_y(double _y);
    double get_x();
    double get_y();
};
