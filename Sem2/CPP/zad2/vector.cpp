#include "vector.h"


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
