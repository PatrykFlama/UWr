#pragma once
#include <bits/stdc++.h>
using namespace std;

#ifndef DOUBLE_EPSILON
#define DOUBLE_EPSILON
const double double_epsilon = 0.000000000005;
#endif

#include "point.h"
#include "line.h"

class Segment{
    private:
    Point a, b;
    public:
    Segment(Point _a, Point _b);
    Segment(const Segment& s);
    void change_a(Point _a);
    void change_b(Point _b);
    Point get_a();
    Point get_b();

    string print();

    double length();
    bool contains(Point p);
    void translation(Point t);

    void rotate(Point o, double angle);

    void point_reflection(Point o);

    void axial_symmetry(Line l);
};