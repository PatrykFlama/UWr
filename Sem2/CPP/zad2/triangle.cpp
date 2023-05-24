#include "triangle.h"

/* #region -------Triangle------- */
Triangle::Triangle(Point _a, Point _b, Point _c) : a(_a), b(_b), c(_c) {
    if(!check_if_correct()) throw invalid_argument("Such triangle is not correct!\n");
}
Triangle::Triangle(const Triangle& t){
    a = t.a, b = t.b, c = t.c;
}

Point Triangle::get_a(){
    return a;
}
Point Triangle::get_b(){
    return b;
}
Point Triangle::get_c(){
    return c;
}

void Triangle::change_a(Point _a){
    a = _a;
}
void Triangle::change_b(Point _b){
    b = _b;
}
void Triangle::change_c(Point _c){
    c = _c;
}


string Triangle::print(){
    string s = "[";
    s += a.print();
    s += "; ";
    s += b.print();
    s += "; ";
    s += c.print();
    s += "]";
    return s;
}


bool Triangle::check_if_correct(){
    if(a != b && b != c) return true;
    return false;
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
