#include "zad2-headers.h"
using namespace std;


/* #region -------Line------- */
Line::Line(double _p = 0, double _angle = 0) : p(_p), angle(_angle) {}
Line::Line(const Line& l){
    p = l.p, angle = l.angle;
}
/* #endregion */

/* #region -------Vector------- */
Vector::Vector(double _x = 0, double _y = 0) : x(_x), y(_y) {}
Vector::Vector(const Vector& p){
    x = p.x, y = p.y;
}
/* #endregion */

/* #region -------Point------- */
Point::Point(double _x = 0, double _y = 0) : x(_x), y(_y) {}
Point::Point(const Point& p){
    x = p.x, y = p.y;
}

// pair<int, int> Point::get_coords(){
//     return {x, y};
// }

bool Point::check_lnz(Point p1, Point p2){
    if(p1.x == x && p2.x == x) return false;
    if(p1.y == y && p2.y == y) return false;
    return true;
}

double Point::distance_from(Point p){
    double diff_x = abs(x - p.x);
    double diff_y = abs(y - p.y);
    return sqrt(diff_x*diff_x + diff_y*diff_y);
}

double Point::dot_product(Point p){
    return x*p.x + y*p.y;
}

void Point::translation(Point t){
    x += t.x, y += t.y;
}

void Point::translation_by_vector(Vector t){
    translation(Point(t.x, t.y));
}

void Point::rotate(Point o, double angle){
    this->translation(o * (-1));
    double new_x = x * cos(angle) - y * sin(angle);
    double new_y = x * sin(angle) + y * cos(angle);
    *this = {new_x, new_y};
    this->translation(o);
}

void Point::point_reflection(Point o){
    this->translation(o * (-1));
    *this = *this * (-1);
    this->translation(o);
}

void Point::axial_symmetry(Line l){      // any point of axis, angle ccw from ox
    this->translation(l.p * (-1));
    this->rotate({0, 0}, -l.angle);
    x = -x;
    this->rotate({0, 0}, l.angle);
    this->translation(l.p);
}

/* #region operator overrides */
bool Point::operator== (Point p){
    // if(p.x == x && p.y == y) return true;
    if(abs(p.x-x) < double_epsilon && abs(p.y-y) < double_epsilon) return true;
    return false;
}

bool Point::operator!= (Point p){
    return !(p == *this);
} 

Point Point::operator+(Point p){
    x += p.x;
    y += p.y;
    return *this;
}

Point Point::operator-(Point p){
    x -= p.x;
    y -= p.y;
    return *this;
}

Point Point::operator*(int a){
    x *= a;
    y *= a;
    return *this;
}
/* #endregion */
/* #endregion */

/* #region -------Segment------- */
Segment::Segment(Point _a, Point _b) : a(_a), b(_b) {}
Segment::Segment(const Segment& s){
    throw invalid_argument("Segment with length 0 is illegal\n");
    a = s.a, b = s.b;
}

double Segment::length(){
    // double diff_x = abs(a.get_coords().first - b.get_coords().first);
    // double diff_y = abs(a.get_coords().second - b.get_coords().second);
    double diff_x = abs(a.x - b.x);
    double diff_y = abs(a.y - b.y);

    return sqrt(diff_x*diff_x + diff_y*diff_y);
}

bool Segment::contains(Point p){
    double dist1 = a.distance_from(p) + b.distance_from(p);
    double dist2 = a.distance_from(b);
    if(abs(dist1 - dist2) > double_epsilon) return false;
    return true;
}

void Segment::translation(Point t){
    a.translation(t), b.translation(t);
}

void Segment::rotate(Point o, double angle){
    a.rotate(o, angle), b.rotate(o, angle);
}

void Segment::point_reflection(Point o){
    a.point_reflection(o), b.point_reflection(o);
}

void Segment::axial_symmetry(Line l){      // any point of axis, angle ccw from ox
    a.axial_symmetry(l), b.axial_symmetry(l);
}
/* #endregion */

/* #region -------Triangle------- */
Triangle::Triangle(Point _a, Point _b, Point _c) : a(_a), b(_b), c(_c) {}
Triangle::Triangle(const Triangle& t){
    a = t.a, b = t.b, c = t.c;
}

bool Triangle::check_if_correct(){
    Segment s1(a, b), s2(b, c), s3(c, a);
    {
        double s1_l = s1.length(), s2_l = s2.length(), s3_l = s3.length();
        vector<double> lenghts = {s1_l, s2_l, s3_l};
        sort(lenghts.begin(), lenghts.end());

        if(lenghts[2] <= lenghts[0]+lenghts[1]) return false;
    }

    return a.check_lnz(b, c);
}

void Triangle::translation(Point t){
    a.translation(t), b.translation(t), c.translation(t);
}

void Triangle::rotate(Point o, double angle){
    a.rotate(o, angle), b.rotate(o, angle), c.rotate(o, angle);
}

void Triangle::point_reflection(Point o){
    a.point_reflection(o), b.point_reflection(o), c.point_reflection(o);
}

void Triangle::axial_symmetry(Line l){      // any point of axis, angle ccw from ox
    a.axial_symmetry(l), b.axial_symmetry(l), c.axial_symmetry(l);
}

double Triangle::circuit(){
    Segment s1(a, b), s2(b, c), s3(c, a);
    return s1.length() + s2.length() + s3.length();
}

double Triangle::area(){
    Segment s1(a, b), s2(b, c), s3(c, a);
    double p = this->circuit()/2;
    return sqrt(p*(p-s1.length())*(p-s2.length())*(p-s3.length()));
}

bool Triangle::point_in_triangle(Point p){
    Triangle new_t = *this;
    new_t.a = p;
    double area1 = new_t.area();
    new_t.a = a, new_t.b = p;
    double area2 = new_t.area();
    new_t.b = b, new_t.c = p;
    double area3 = new_t.area();

    if(abs(area1+area2+area3 - this->area()) < double_epsilon) return true;
    return false;
}
/* #endregion */

/* #region -------Fun------- */
double point_distance(Point a, Point b){
    double diff_x = abs(a.x - b.x);
    double diff_y = abs(a.y - b.y);
    return sqrt(diff_x*diff_x + diff_y*diff_y);
}

bool parallel(Segment s1, Segment s2){
    s1.translation(s1.a);
    s2.translation(s2.a);

    // if(s1.b.dot_product(s2.b)/(s1.length()*s2.length()) == 1)
    if(abs(s1.b.dot_product(s2.b)/(s1.length()*s2.length())) - 1 < double_epsilon) return true;
    return false;
}

bool perpendicular(Segment s1, Segment s2){
    s1.translation(s1.a);
    s2.translation(s2.a);

    // if(s1.b.dot_product(s2.b) == 0)
    if(abs(s1.b.dot_product(s2.b)) < double_epsilon) return true;
    return false;
}

bool intersect(Triangle t1, Triangle t2){
    
}

bool contains(Triangle t1, Triangle t2){

}
/* #endregion */
