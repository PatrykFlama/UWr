#include <bits/stdc++.h>
using namespace std;

typedef float color_value;


class Color{
protected:
    color_value _r, _g, _b;

    inline bool out_of_range(color_value col) const {
        return (col < 0 || col > 255);
    }

public:
    Color(color_value r = 0, color_value g = 0, color_value b = 0){
        if(out_of_range(r) || out_of_range(g) || out_of_range(b))
            throw invalid_argument("Color value out of range");
        
        _r = r;
        _g = g;
        _b = b;
    }

    color_value get_r() const { return _r; }
    color_value get_g() const { return _g; }
    color_value get_b() const { return _b; }

    void set_r(color_value r){ 
        if(out_of_range(r)) throw invalid_argument("Color value out of range");
        _r = r; 
    }
    void set_g(color_value g){ 
        if(out_of_range(g)) throw invalid_argument("Color value out of range");
        _g = g; 
    }
    void set_b(color_value b){ 
        if(out_of_range(b)) throw invalid_argument("Color value out of range");
        _b = b; 
    }

    void lighten(color_value diff){
        _r = min(color_value(255), _r+diff);
        _g = min(color_value(255), _g+diff);
        _b = min(color_value(255), _b+diff);
    }

    void darken(color_value diff){
        _r = max(color_value(0), _r-diff);
        _g = max(color_value(0), _g-diff);
        _b = max(color_value(0), _b-diff);
    }

    static color_value combine(color_value a, color_value b) {
        return (a+b)/2;
    }
};

class TransparentColor : public virtual Color{
protected:
    color_value _alpha;

public:
    TransparentColor(color_value r = 0, color_value g = 0, color_value b = 0, color_value alpha = 255) : Color(r, g, b){
        if(out_of_range(alpha)) throw invalid_argument("Alpha channel value out of range");
        _alpha = alpha;
    }

    color_value get_transparency() const { return _alpha; }
    void set_transparency(color_value alpha){
        if(out_of_range(alpha)) throw invalid_argument("Alpha channel value out of range");
        _alpha = alpha;
    }
};

class NamedColor : public virtual Color{
protected:
    string _name;

    bool check_name(string &name) const {
        for(auto i : name)
            if(i > 'z' || i < 'a') return false;
        return true;
    }

public:
    NamedColor(color_value r = 0, color_value g = 0, color_value b = 0, string name = "") : Color(r, g, b){
        if(check_name(name)) throw invalid_argument("Invalid name (should consist only of small letters)");
        _name = name;
    }

    string get_name() const { return _name; }
    void set_name(string name){
        if(check_name(name)) throw invalid_argument("Invalid name (should consist only of small letters)");
        _name = name;
    }
};

//* https://isocpp.org/wiki/faq/multiple-inheritance#mi-diamond
class TransNamColor : public TransparentColor, public NamedColor{
public:
    TransNamColor(color_value r = 0, color_value g = 0, color_value b = 0,
                  color_value alpha = 255, string name = "")
                  : TransparentColor(r, g, b, alpha){
        if(check_name(name)) throw invalid_argument("Invalid name (should consist only of small letters)");
        _name = name;
    }
};

class Pixel{
protected:
    const static int size_x = 1980, size_y = 1080;      // which defaults to static at namespace level
    int _x, _y;

    inline bool check_pixels(int x, int y) const {
        return (x < 0 || size_x <= x || y < 0 || y <= size_y);
    }

public:
    Pixel(int x, int y){
        if(check_pixels(x, y)) throw invalid_argument("Pixels are out of the screen borders");
        _x = x;
        _y = y;
    }

    int get_x() const { return _x; }
    int get_y() const { return _y; }

    float calc_dist(int x, int y) const {
        return abs(sqrt((_x-x)*(_x-x) + (_y-y)*(_y-y)));
    }

    inline float calc_dist_left_up()    const { return calc_dist(0, 0); }
    inline float calc_dist_right_up()   const { return calc_dist(size_x, 0); }
    inline float calc_dist_left_down()  const { return calc_dist(0, size_y); }
    inline float calc_dist_right_down() const { return calc_dist(size_x, size_y); }
};

class ColorPixel : public Pixel, public TransparentColor{
public:
    ColorPixel(int x, int y, color_value r = 0, color_value g = 0, color_value b = 0, color_value alpha = 255)
        : Pixel(x, y), TransparentColor(r, g, b, alpha){}
    
    void transform(int dx, int dy){
        _x += dx;
        _y += dy;
        if(check_pixels(_x, _y)) throw invalid_argument("Pixels are out of the screen borders");
    }
};

int pixel_distance(const Pixel &p, const Pixel &q){
    return (int)(p.calc_dist(q.get_x(), q.get_y()));
}
int pixel_distance(const Pixel *p, const Pixel *q){
    return (int)(p->calc_dist(q->get_x(), q->get_y()));
}


int main(){

}
