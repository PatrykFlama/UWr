#pragma once
#include <bits/stdc++.h>
using namespace std;

#ifndef DOUBLE_EPSILON
#define DOUBLE_EPSILON
const double double_epsilon = 0.000000000005;
#endif

#include "line.h"
#include "vector.h"

class Point{
    private:
    double x, y;
    public:
    Point();
    Point(double _x, double _y);
    Point(const Point& p);
    void change_x(double _x);
    void change_y(double _y);
    double get_x();
    double get_y();

    string print();

    bool check_lnz(Point p1, Point p2);

    double distance_from(Point p);

    double dot_product(Point p);

    void translation(Point t);
    void translation_by_vector(Vector t);

    void rotate(Point o, double angle);

    void point_reflection(Point o);

    void axial_symmetry(Line l);

    // operator overrides
    bool operator== (Point p);
    bool operator!= (Point p);

    Point operator+(Point p);

    Point operator-(Point p);

    Point operator*(int a);
};
