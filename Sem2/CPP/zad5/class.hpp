#pragma once
#include <bits/stdc++.h>
using namespace std;

// typedef float color_value;
#define color_value float


class Color{
protected:
    color_value _r, _g, _b;

    inline bool out_of_range(color_value col) const {
        return (col < 0 || col > 255);
    }

public:
    Color(color_value r, color_value g, color_value b);

    color_value get_r() const;
    color_value get_g() const;
    color_value get_b() const;

    void set_r(color_value r);
    void set_g(color_value g);
    void set_b(color_value b);

    void lighten(color_value diff);

    void darken(color_value diff);

    static color_value combine(color_value a, color_value b);

    friend ostream &operator<< (ostream &out, const Color &c);
};

class TransparentColor : public virtual Color{
protected:
    color_value _alpha;

public:
    TransparentColor(color_value r, color_value g, color_value b, color_value alpha);

    color_value get_transparency() const;
    void set_transparency(color_value alpha);

    friend ostream &operator<< (ostream &out, const TransparentColor &c);
};

class NamedColor : public virtual Color{
protected:
    string _name;

    bool check_name(string &name) const;

public:
    NamedColor(color_value r, color_value g, color_value b, string name);

    string get_name() const;
    void set_name(string name);

    friend ostream &operator<< (ostream &out, const NamedColor &c);
};

//* https://isocpp.org/wiki/faq/multiple-inheritance#mi-diamond
class TransNamColor : public TransparentColor, public NamedColor{
public:
    TransNamColor(color_value r, color_value g, color_value b,
                  color_value alpha, string name);

    friend ostream &operator<< (ostream &out, const TransNamColor &c);
};

class Pixel{
protected:
    const static int size_x = 1980, size_y = 1080;      // which defaults to static at namespace level
    int _x, _y;

    inline bool check_pixels(int x, int y) const {
        return (x < 0 || size_x <= x || y < 0 || size_y <= y);
    }

public:
    Pixel(int x, int y);

    int get_x() const;
    int get_y() const;

    float calc_dist(int x, int y) const;

    inline int calc_dist_left_up()    const { return calc_dist(0, 0); }
    inline int calc_dist_right_up()   const { return calc_dist(size_x, 0); }
    inline int calc_dist_left_down()  const { return calc_dist(0, size_y); }
    inline int calc_dist_right_down() const { return calc_dist(size_x, size_y); }

    friend ostream &operator<< (ostream &out, const Pixel &c);
};

class ColorPixel : public Pixel, public TransparentColor{
public:
    ColorPixel(int x, int y, color_value r, color_value g, color_value b, color_value alpha);
    
    void transform(int dx, int dy);

    friend ostream &operator<< (ostream &out, const ColorPixel &c);
};

int pixel_distance(const Pixel &p, const Pixel &q);
int pixel_distance(const Pixel *p, const Pixel *q);
