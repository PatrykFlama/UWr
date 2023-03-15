#include "zad2-headers.h"


/* #region -------Line------- */
Line::Line(double _p = 0, double _angle = 0) : p(_p), angle(_angle) {}
Line::Line(const Line& l){
    p = l.p, angle = l.angle;
}
void Line::change_p(double _p){
    p = _p;
}
void Line::change_angle(double _angle){
    angle = _angle;
}
double Line::get_p(){
    return p;
}
double Line::get_angle(){
    return angle;
}
/* #endregion */

/* #region -------Vector------- */
Vector::Vector(double _x = 0, double _y = 0) : x(_x), y(_y) {}
Vector::Vector(const Vector& p){
    x = p.x, y = p.y;
}
void Vector::change_x(double _x){
    x = _x;
}
void Vector::change_y(double _y){
    y = _y;
}
double Vector::get_x(){
    return x;
}
double Vector::get_y(){
    return y;
}
/* #endregion */

/* #region -------Point------- */
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
    s += x; s += ", "; s += y; s += ")";
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
    double diff_x = abs(a.get_x() - b.get_x());
    double diff_y = abs(a.get_y() - b.get_y());
    return sqrt(diff_x*diff_x + diff_y*diff_y);
}

bool parallel(Segment s1, Segment s2){
    s1.translation(s1.get_a());
    s2.translation(s2.get_a());

    // if(s1.b.dot_product(s2.b)/(s1.length()*s2.length()) == 1)
    if(abs(s1.get_b().dot_product(s2.get_b())/(s1.length()*s2.length())) - 1 < double_epsilon) return true;
    return false;
}

bool perpendicular(Segment s1, Segment s2){
    s1.translation(s1.get_a());
    s2.translation(s2.get_a());

    // if(s1.b.dot_product(s2.b) == 0)
    if(abs(s1.get_b().dot_product(s2.get_b())) < double_epsilon) return true;
    return false;
}

bool ccw(Point a, Point b, Point c){
    return (c.get_y() - a.get_y()) * (b.get_x() - a.get_x()) > (b.get_y() - a.get_y()) * (c.get_x() - a.get_x());
}

bool intersect_seg(Segment a, Segment b){
    return (ccw(a.get_a(), b.get_a(), b.get_b()) != ccw(a.get_b(), b.get_a(), b.get_b())) and
           (ccw(a.get_a(), a.get_b(), b.get_a()) != ccw(a.get_a(), a.get_b(), b.get_b()));
}

bool intersect(Triangle t1, Triangle t2){
    Segment s1a(t1.get_a(), t1.get_b()),
            s1b(t1.get_b(), t1.get_c()),
            s1c(t1.get_c(), t1.get_a());
    Segment s2a(t2.get_a(), t2.get_b()),
            s2b(t2.get_b(), t2.get_c()),
            s2c(t2.get_c(), t2.get_a());

    return (intersect_seg(s1a, s2a) or intersect_seg(s1a, s2b) or intersect_seg(s1a, s2c) or
            intersect_seg(s1b, s2a) or intersect_seg(s1b, s2b) or intersect_seg(s1b, s2c) or
            intersect_seg(s1c, s2a) or intersect_seg(s1c, s2b) or intersect_seg(s1c, s2c));            
}

bool contains(Triangle t1, Triangle t2){
    if(intersect(t1, t2)) return false;
    // for every 2 points on t1, every point in t2 is in same orientation
    int temp = 0;
    if(ccw(t1.get_a(), t1.get_b(), t2.get_a())) temp ++; else temp--;
    if(ccw(t1.get_a(), t1.get_b(), t2.get_b())) temp ++; else temp--;
    if(ccw(t1.get_a(), t1.get_b(), t2.get_c())) temp ++; else temp--;

    if(ccw(t1.get_b(), t1.get_c(), t2.get_a())) temp ++; else temp--;
    if(ccw(t1.get_b(), t1.get_c(), t2.get_b())) temp ++; else temp--;
    if(ccw(t1.get_b(), t1.get_c(), t2.get_c())) temp ++; else temp--;

    if(ccw(t1.get_c(), t1.get_a(), t2.get_a())) temp ++; else temp--;
    if(ccw(t1.get_c(), t1.get_a(), t2.get_b())) temp ++; else temp--;
    if(ccw(t1.get_c(), t1.get_a(), t2.get_c())) temp ++; else temp--;

    if(abs(temp) == 9) return true;
    return false;
}
/* #endregion */

