#include <bits/stdc++.h>
using namespace std;
const double double_epsilon = 0.000000000005;


class Line{
    double p, angle;
    public:
    Line(double _p, double _angle);
    Line(const Line& l);
    void change_p(double _p);
    void change_angle(double _angle);
    double get_p();
    double get_angle();
};

class Vector{
    double x, y;
    public:
    Vector(double _x, double _y);
    Vector(const Vector& p);
    void change_x(double _x);
    void change_y(double _y);
    double get_x();
    double get_y();
};

class Point{
    double x, y;
    public:
    Point(double _x, double _y);
    Point(const Point& p);
    void change_x(double _x);
    void change_y(double _y);
    double get_x();
    double get_y();

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

class Segment{
    Point a, b;
    public:
    Segment(Point _a, Point _b);
    Segment(const Segment& s);
    void change_a(Point _a);
    void change_b(Point _b);
    Point get_a();
    Point get_b();

    double length();
    bool contains(Point p);
    void translation(Point t);

    void rotate(Point o, double angle);

    void point_reflection(Point o);

    void axial_symmetry(Line l);
};

class Triangle{
    Point a, b, c;

    public:
    Triangle(Point _a, Point _b, Point _c);
    Triangle(const Triangle& t);

    Point get_a();
    Point get_b();
    Point get_c();

    bool check_if_correct();

    void translation(Point t);

    void rotate(Point o, double angle);

    void point_reflection(Point o);

    void axial_symmetry(Line l);

    double circuit();

    double area();

    bool point_in_triangle(Point p);
};


double point_distance(Point a, Point b);
bool parallel(Segment s1, Segment s2);

bool perpendicular(Segment s1, Segment s2);

bool intersect_seg(Segment a, Segment b);
bool intersect(Triangle t1, Triangle t2);

bool contains(Triangle t1, Triangle t2);