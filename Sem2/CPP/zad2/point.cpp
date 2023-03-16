#include "point.h"

/* #region -------Point------- */
Point::Point() {Point(0, 0);}
Point::Point(double _x = 0, double _y = 0) : x(_x), y(_y) {}
Point::Point(const Point& p){
    x = p.x, y = p.y;
}
void Point::change_x(double _x){
    x = _x;
}
void Point::change_y(double _y){
    y = _y;
}
double Point::get_x(){
    return x;
}
double Point::get_y(){
    return y;
}

string Point::print(){
    string s = "(";
    s += to_string(x); s += ", "; s += to_string(y); s += ")";
    return s;
}

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
    translation(Point(t.get_x(), t.get_y()));
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
    this->translation(l.get_p() * (-1));
    this->rotate({0, 0}, -l.get_angle());
    x = -x;
    this->rotate({0, 0}, l.get_angle());
    this->translation(l.get_p());
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
