#include "line.h"

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
