#pragma once
#include <bits/stdc++.h>
using namespace std;

#ifndef DOUBLE_EPSILON
#define DOUBLE_EPSILON
const double double_epsilon = 0.000000000005;
#endif

#include "point.h"
#include "segment.h"
#include "line.h"


class Triangle{
    private:
    Point a, b, c;

    public:
    Triangle(Point _a, Point _b, Point _c);
    Triangle(const Triangle& t);

    Point get_a();
    Point get_b();
    Point get_c();
    void change_a(Point _a);
    void change_b(Point _b);
    void change_c(Point _c);

    string print();

    bool check_if_correct();

    void translation(Point t);

    void rotate(Point o, double angle);

    void point_reflection(Point o);

    void axial_symmetry(Line l);

    double circuit();

    double area();

    bool point_in_triangle(Point p);
};
