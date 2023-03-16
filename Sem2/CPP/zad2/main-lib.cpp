#include "main.h"


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

bool does_not_intersect(Triangle t1, Triangle t2){
    return !contains(t1, t2) and !intersect(t1, t2);
}
/* #endregion */
