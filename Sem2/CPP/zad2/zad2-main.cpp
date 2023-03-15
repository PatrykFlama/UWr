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
    cout << "----END OF DEMO----\n";

    p0 = p1 = p2 = Point(0,0);
    s1 = s2 = Segment(Point(0, 0), Point(1, 0));
    Triangle t2 = t1;
    cout << "You start with 3 points pi (i from 0 to 2) set to 0\n";
    cout << "2 segments si (1, 2) set to 0,0 1,0\n";
    cout << "and 2 triangles ti (1, 2) set to 0,0 2,2 4,0\n";
    cout << "to execute operation type in:\n";
    cout << "[object] [operation]\n";
    cout << "objects: {point, segment, triangle} {index}\n";
    cout << "operations for all: {translate vector, rotate point angle, point_reflect point, axial_symmetry line_point line_angle}\n";
    cout << "operations for point: {dot_product point, distance_from point}\n";
    cout << "operations for segment: {length, contains point, intersects segment, perpendicular segment}\n";
    cout << "operations for triangle: {circuit, area, point_in_triangle point, intersect triangle, contains triangle}\n";
    cout << "type 'print' to print and object\n";
    cout << "type 'exit' to exit" << endl;

    string object = "", operation = "";
    int index = 0;
    while(object != "exit"){
        cin >> object;
        if(object == "exit") break;
        cin >> index >> operation;

        if(object[0] == 'p'){       // point
            Point* act;
            if(index == 0) act = &p0;
            else if(index == 1) act = &p1;
            else act = &p2;

            if(operation == "translate"){
                float x, y;
                cout << "Give x y coords for vector: ";
                cin >> x >> y;
                Point tran(x, y);
                act->translation(tran);
            } else if(operation == "rotate"){
                float x, y, angle;
                cout << "Give x y for point and angle: ";
                cin >> x >> y >> angle;
                Point O(x, y);
                act->rotate(O, angle);
            } else if(operation == "point_reflect"){
                float x, y;
                cout << "Give x y for point: ";
                cin >> x >> y;
                act->point_reflection(Point(x, y));
            } else if(operation == "axial_symmetry"){
                float x, angle;
                cout << "Give x for axis offset and angle: ";
                cin >> x>>angle;
                act->axial_symmetry(Line(x, angle));
            } else if(operation == "print"){
                // TODO print obj
            }
        } else if(object[0] == 's'){        // segment
            Segment* act;
            if(index == 1) act = &s1;
            else act = &s2;
            if(operation == "translate"){
                float x, y;
                cout << "Give x y coords for vector: ";
                cin >> x >> y;
                Point tran(x, y);
                act->translation(tran);
            } else if(operation == "rotate"){
                float x, y, angle;
                cout << "Give x y for point and angle: ";
                cin >> x >> y >> angle;
                Point O(x, y);
                act->rotate(O, angle);
            } else if(operation == "point_reflect"){
                float x, y;
                cout << "Give x y for point: ";
                cin >> x >> y;
                act->point_reflection(Point(x, y));
            } else if(operation == "axial_symmetry"){
                float x, angle;
                cout << "Give x for axis offset and angle: ";
                cin >> x>>angle;
                act->axial_symmetry(Line(x, angle));
            } else if(operation == "print"){
                // TODO print obj
            }
        } else if(object[0] == 't'){       // triangle
            Triangle* act;
            if(index == 1) act = &t1;
            else act = &t2;
            if(operation == "translate"){
                float x, y;
                cout << "Give x y coords for vector: ";
                cin >> x >> y;
                Point tran(x, y);
                act->translation(tran);
            } else if(operation == "rotate"){
                float x, y, angle;
                cout << "Give x y for point and angle: ";
                cin >> x >> y >> angle;
                Point O(x, y);
                act->rotate(O, angle);
            } else if(operation == "point_reflect"){
                float x, y;
                cout << "Give x y for point: ";
                cin >> x >> y;
                act->point_reflection(Point(x, y));
            } else if(operation == "axial_symmetry"){
                float x, angle;
                cout << "Give x for axis offset and angle: ";
                cin >> x>>angle;
                act->axial_symmetry(Line(x, angle));
            } else if(operation == "print"){
                // TODO print obj
            }
        }
    }
}
