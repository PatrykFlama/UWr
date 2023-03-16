#pragma once
#include <bits/stdc++.h>

#ifndef DOUBLE_EPSILON
#define DOUBLE_EPSILON
const double double_epsilon = 0.000000000005;
#endif

class Line{
    private:
    double p, angle;
    public:
    Line(double _p, double _angle);
    Line(const Line& l);
    void change_p(double _p);
    void change_angle(double _angle);
    double get_p();
    double get_angle();
};