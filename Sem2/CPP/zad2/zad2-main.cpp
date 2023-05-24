#include "main.h"


int main(){
    cout << "Enter 1 for short demo and 2 for manual input: ";
    int choice; cin >> choice;
    
    //* --------------------------------------------------------------------
    if(choice == 1){
    
    Point p0(0, 0), p1(2, 2), p2(4, 0);
    Segment s1(p0, p1), s2(Point(0, 2), Point(2, 0));
    Triangle t1(p0, p1, p2);

    cout << "length of 0,0 2,2: " << s1.length() << '\n';
    cout << "distance between 0,0 2,2: " << p0.distance_from(p1) << '\n';
    cout << "dot product between 4,0 2,2: " << p2.dot_product(p1) << '\n';

    cout << "does 0,0 2,2 intersect 2,0 0,2: " << intersect_seg(s1, s2) << '\n';
    cout << "does 0,0 2,2 intersect 2,0 4,0: " << intersect_seg(s1, Segment(Point(2, 0), Point(4, 0))) << '\n';
    cout << "does 0,0 2,2 intersect 2,0 4,1: " << intersect_seg(s1, Segment(Point(2, 0), Point(4, 1))) << '\n';

    s1.rotate(Point(0,0), 3.14);
    cout << "rotationg segment 1 by point 0,0: " << s1.print() << '\n';
    s1.rotate(Point(0,0), 3.14);
    cout << "rotationg segment 1 by point 0,0: " << s1.print() << '\n';
    s1.rotate(Point(0,0), 3.14/2);
    cout << "rotationg segment 1 by point 0,0: " << s1.print() << '\n';

    t1.rotate(Point(0,0), 3.14);
    cout << "rotationg triangle 1 pi by point 0,0: " << t1.print() << '\n';
    t1.rotate(Point(0,0), 3.14);
    cout << "rotationg triangle 1 pi by point 0,0: " << t1.print() << '\n';
    t1.rotate(Point(0,0), 3.14/2);
    cout << "rotationg triangle 1 pi/2 by point 0,0: " << t1.print() << '\n';
    
    t1.axial_symmetry(Line(0, 3.14/4));
    cout << "axial symetry of triangle 1 pi/4: " << t1.print() << '\n';
    t1.axial_symmetry(Line(0, 3.14/4));
    cout << "axial symetry of triangle 1 pi/4: " << t1.print() << '\n';
    t1.point_reflection(Point(0, 0));
    cout << "axial point reflection 0,0 of triangle 1: " << t1.print() << '\n';

    cout << "does triangle 0,0 2,2 4,0 contain 0.5,0.5 1,1 3,0.5: ";
    cout << contains(t1, Triangle(Point(0.5,0.5), Point(1,1), Point(3,0.5))) << '\n';
    cout << "does triangle 0,0 2,2 4,0 intersect with 0.5,0.5 1,1 3,0.5: ";
    cout << does_not_intersect(t1, Triangle(Point(0.5,0.5), Point(1,1), Point(3,0.5))) << '\n';
    cout << "does triangle 0,0 2,2 4,0 contain itself: ";
    cout << contains(t1, t1) << '\n';

    cout << "\nOperations on points:\n";
    cout << p1.print() << '+' << p2.print() << " = " << (p1+p2).print() << '\n';
    cout << p1.print() << '-' << p2.print() << " = " << (p1-p2).print() << '\n';
    cout << p1.print() << '*' << 5 << " = " << (p1*5).print() << '\n';
    cout << p1.print() << "==" << p2.print() << " = " << (p1==p2) << '\n';
    cout << p1.print() << "!=" << p2.print() << " = " << (p1!=p2) << '\n';

    cout << "\nOperations on segments:\n";
    cout << s1.print() << " len = " << s1.length() << '\n';
    s1.rotate(Point(0,0), 3.14/4);
    cout << "rotate by pi/4 with 0,0 " << s1.print() << '\n';
    s1.point_reflection(p0);
    cout << "point reflection with " << p0.print() << ": " << s1.print() << '\n';
    s1.axial_symmetry(Line(0, 3.14/4));
    cout << "axial symmetry by axis with angle pi/4 ccw: " << s1.print() << '\n';

    cout << "\nOperations on triangles:\n";
    cout << "Out triangle: " << t1.print() << '\n';
    cout << "Is triangle correct? " << t1.check_if_correct() << '\n';
    t1.rotate(Point(1, 1), 3.14/2);
    cout << "Rotate by pi/2 with anchor in point 1,1: " << t1.print() << '\n';
    t1.point_reflection(p0);
    cout << "point reflection with " << p0.print() << ": " << t1.print() << '\n';
    t1.axial_symmetry(Line(0, 3.14/4));
    cout << "axial symmetry by axis with angle pi/4 ccw: " << t1.print() << '\n';
    cout << "area: " << t1.area();
    cout << "\ncircuit: " << t1.circuit() << '\n';

    cout << "----END OF DEMO----\n";
    }
    //* --------------------------------------------------------------------
    else if(choice == 2){
    Point p0(0,0), p1(0,0), p2(0,0); 
    Segment s1(Point(0, 0), Point(1, 0)), s2(Point(0, 0), Point(1, 0));
    Triangle t1(Point(0, 0), Point(2,2), Point(4, 0)), t2(Point(0, 0), Point(2,2), Point(4, 0));

    /* #region INFO */
    cout << "You start with 3 points pi (indexes i from 0 to 2)\n";
    cout << "2 segments si (1, 2)\n";
    cout << "and 2 triangles ti (1, 2)\n";
    cout << "to execute operation type in:\n";
    cout << "[object] [index] [operation]\n";
    cout << "objects: {point, segment, triangle}\n";
    cout << "operations for all: {print, set, translate vector, rotate point angle, point_reflect point, axial_symmetry line_point line_angle}\n";
    cout << "operations for point: {dot_product point, distance_from point}\n";
    cout << "operations for segment: {length, contains point, intersects segment, perpendicular segment}\n";
    cout << "operations for triangle: {circuit, area, point_in_triangle point, intersect triangle, contains triangle, does_not_intersect triangle}\n";
    cout << "type 'exit' to exit" << endl;
    /* #endregion */

    string object = "", operation = "";
    int index = 0;
    while(object != "exit"){
        cout << "-> ";
        cin >> object;
        if(object == "exit") break;
        cin >> index >> operation;

        if(object[0] == 'p'){       // point
            Point* act;
            if(index == 0) act = &p0;
            else if(index == 1) act = &p1;
            else act = &p2;

            if(operation == "set"){
                float x, y;
                cout << "Give x y coords for new point: ";
                cin >> x >> y;
                act->change_x(x), act->change_y(y);
            } else if(operation == "translate"){
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
                cout << act->print() << '\n';
            } else if(operation == "dot_product point"){
                float x, y;
                cout << "Give x y for point: ";
                cin >> x >> y;
                cout << act->dot_product(Point(x, y)) << '\n';
            } else if(operation == "distance_from point"){
                float x, y;
                cout << "Give x y for point: ";
                cin >> x >> y;
                cout << act->distance_from(Point(x, y)) << '\n';
            }
        } else if(object[0] == 's'){        // segment
            Segment* act;
            if(index == 1) act = &s1;
            else act = &s2;

            if(operation == "set"){
                float x1, y1, x2, y2;
                cout << "Give x1 y1 x2 y2 coords for new points: ";
                cin >> x1 >> y1 >> x2 >> y2;
                act->change_a(Point(x1, y1)), act->change_b(Point(x2, y2));
            } else if(operation == "translate"){
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
                cout << act->print() << '\n';
            } else if(operation == "length"){
                cout << act->length() << '\n';
            } else if(operation == "contains"){
                float x, y;
                cout << "Give x y for point: ";
                cin >> x >> y;
                cout << act->contains(Point(x, y)) << '\n';
            } else if(operation == "intersects"){
                float x1, y1;
                float x2, y2;
                cout << "Give x1 y1 x2 y2 for segment points: ";
                cin >> x1 >> y1 >> x2 >> y2;
                cout << intersect_seg(*act, Segment(Point(x1, y1), Point(x2, y2))) << '\n';
            } else if(operation == "perpendicular"){
                float x1, y1;
                float x2, y2;
                cout << "Give x1 y1 x2 y2 for segment points: ";
                cin >> x1 >> y1 >> x2 >> y2;
                cout << perpendicular(*act, Segment(Point(x1, y1), Point(x2, y2))) << '\n';
            }
        } else if(object[0] == 't'){       // triangle
            Triangle* act;
            if(index == 1) act = &t1;
            else act = &t2;
            
            if(operation == "set"){
                float x1, y1, x2, y2, x3, y3;
                cout << "Give x1 y1 x2 y2 x3 y3 coords for new points: ";
                cin >> x1 >> y1 >> x2 >> y2 >> x3 >> y3;
                act->change_a(Point(x1, y1)), act->change_b(Point(x2, y2)), act->change_c(Point(x3, y3));
            } else if(operation == "translate"){
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
                cout << act->print() << '\n';
            } else if(operation == "circuit"){
                cout << act->circuit() << '\n';
            } else if(operation == "area"){
                cout << act->area() << '\n';
            } else if(operation == "point_in_triangle"){
                float x, y;
                cout << "Give x y for point: ";
                cin >> x >> y;
                cout << act->point_in_triangle(Point(x, y)) << '\n';
            } else if(operation == "intersect triangle"){
                float x1, y1, x2, y2, x3, y3;
                cout << "Give x1 y1 x2 y2 x3 y3 for triangle points: ";
                cin >> x1 >> y1 >> x2 >> y2 >> x3 >> y3;
                cout << intersect(*act, Triangle(Point(x1, y1), Point(x2, y2), Point(x3, y3))) << '\n';
            } else if(operation == "contains triangle"){
                float x1, y1, x2, y2, x3, y3;
                cout << "Give x1 y1 x2 y2 x3 y3 for triangle points: ";
                cin >> x1 >> y1 >> x2 >> y2 >> x3 >> y3;
                cout << contains(*act, Triangle(Point(x1, y1), Point(x2, y2), Point(x3, y3))) << '\n';
            } else if(operation == "does_not_intersect"){
                float x1, y1, x2, y2, x3, y3;
                cout << "Give x1 y1 x2 y2 x3 y3 for triangle points: ";
                cin >> x1 >> y1 >> x2 >> y2 >> x3 >> y3;
                cout << does_not_intersect(*act, Triangle(Point(x1, y1), Point(x2, y2), Point(x3, y3))) << '\n';

            }
        }
        cout << "done\n";
    }
    }
}
