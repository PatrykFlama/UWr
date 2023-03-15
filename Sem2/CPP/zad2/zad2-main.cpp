#include "zad2-headers.h"


int main(){
    Point p0(0, 0), p1(2, 2), p2(4, 0);
    Segment s1(p0, p1), s2(Point(0, 2), Point(2, 0));
    Triangle t1(p0, p1, p2);

    cout << "length of 0,0 2,2: " << s1.length() << '\n';
    cout << "distance between 0,0 2,2: " << p0.distance_from(p1) << '\n';
    cout << "dot product between 4,0 2,2: " << p2.dot_product(p1) << '\n';

    cout << "does 0,0 2,2 intersect 2,0 0,2: " << intersect_seg(s1, s2) << '\n';
    cout << "does 0,0 2,2 intersect 2,0 4,0: " << intersect_seg(s1, Segment(Point(2, 0), Point(4, 0))) << '\n';
    cout << "does 0,0 2,2 intersect 2,0 4,1: " << intersect_seg(s1, Segment(Point(2, 0), Point(4, 1))) << '\n';

    cout << "does triangle 0,0 2,2 4,0 contain 0.5,0.5 1,1 3,0.5: ";
    cout << contains(t1, Triangle(Point(0.5,0.5), Point(1,1), Point(3,0.5))) << '\n';
    cout << "does triangle 0,0 2,2 4,0 contain itself: ";
    cout << contains(t1, t1) << '\n';
    cout << "----END OF DEMOO----\n";

    p0 = p1 = p2 = Point(0,0);
    s1 = s2 = Segment(Point(0, 0), Point(1, 0));
    Triangle t2 = t1;
    cout << "You start with 3 points pi set to 0\n, 2 segments si set to 0,0 1,0\n";
    cout << "and 2 triangles ti set to 0,0 2,2 4,0\n";
    cout << "to execute operation type in:\n";
    cout << "[object] [operation]\n";
    cout << "operations for all: {translate, rotate, point_reflect, axial_symmetry}\n";
    cout << "operations for point: {dot_product point, distance_from point}\n";
    cout << "operations for segment: {length, contains point, intersects segment}\n";
    cout << "operations for triangle: {}\n";    // tODO
}
