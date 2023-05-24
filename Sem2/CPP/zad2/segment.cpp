#include "segment.h"

/* #region -------Segment------- */
Segment::Segment(Point _a, Point _b) : a(_a), b(_b) {
    if(a == b) throw invalid_argument("Segment with length 0 is illegal\n");
}
Segment::Segment(const Segment& s){
    a = s.a, b = s.b;
}
void Segment::change_a(Point _a){
    a = _a;
}
void Segment::change_b(Point _b){
    b = _b;
}
Point Segment::get_a(){
    return a;
}
Point Segment::get_b(){
    return b;
}

string Segment::print(){
    string s = "[";
    s += a.print();
    s += "; ";
    s += b.print();
    s += "]";
    return s;
}

double Segment::length(){
    // double diff_x = abs(a.get_coords().first - b.get_coords().first);
    // double diff_y = abs(a.get_coords().second - b.get_coords().second);
    double diff_x = abs(a.get_x() - b.get_x());
    double diff_y = abs(a.get_y() - b.get_y());

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
