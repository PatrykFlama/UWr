#include "pixel.hpp"


Pixel::Pixel(int x, int y){
    if(check_pixels(x, y)) throw invalid_argument("Pixels are out of the screen borders");
    _x = x;
    _y = y;
}

int Pixel::get_x() const { return _x; }
int Pixel::get_y() const { return _y; }

float Pixel::calc_dist(int x, int y) const {
    return abs(sqrt((_x-x)*(_x-x) + (_y-y)*(_y-y)));
}

ostream &operator<< (ostream &out, const Pixel &c){
    return out << c.get_x() << ' ' << c.get_y();
}

ColorPixel::ColorPixel(int x, int y, color_value r = 0, color_value g = 0, color_value b = 0, color_value alpha = 255){
    if(check_pixels(x, y)) throw invalid_argument("Pixels are out of the screen borders");
    _x = x;
    _y = y;
    if(out_of_range(alpha)) throw invalid_argument("Alpha channel value out of range");
    _alpha = alpha;
    if(out_of_range(r) || out_of_range(g) || out_of_range(b)) throw invalid_argument("Color value out of range");
    _r = r;
    _g = g;
    _b = b;
}

void ColorPixel::transform(int dx, int dy){
    _x += dx;
    _y += dy;
    if(check_pixels(_x, _y)) throw invalid_argument("Pixels are out of the screen borders");
}

ostream &operator<< (ostream &out, const ColorPixel &c){
    return out << c.get_x() << ' ' << c.get_y() << ' ' << c.get_r() << ' ' << c.get_g() << ' ' << c.get_b() << ' ' << c.get_transparency();
}

int pixel_distance(const Pixel &p, const Pixel &q){
    return (int)(p.calc_dist(q.get_x(), q.get_y()));
}
int pixel_distance(const Pixel *p, const Pixel *q){
    return (int)(p->calc_dist(q->get_x(), q->get_y()));
}
