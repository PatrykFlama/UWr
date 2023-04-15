#pragma once
#include "color.hpp"


class Pixel{
protected:
    const static int size_x = 1980, size_y = 1080;      // which defaults to static at namespace level
    int _x, _y;

    inline bool check_pixels(int x, int y) const {
        return (x < 0 || size_x <= x || y < 0 || size_y <= y);
    }

public:
    Pixel();
    Pixel(int x, int y);

    int get_x() const;
    int get_y() const;

    float calc_dist(int x, int y) const;

    int calc_dist_left()    const { return (int)(_x); }
    int calc_dist_right()   const { return (int)(size_x - _x - 1); }
    int calc_dist_up()  const { return (int)(_y); }
    int calc_dist_down() const { return (int)(size_y - _y - 1); }

    friend ostream &operator<< (ostream &out, const Pixel &c);
};

class ColorPixel : public Pixel, public TransparentColor{
public:
    ColorPixel();
    ColorPixel(int x, int y, color_value r, color_value g, color_value b, color_value alpha);
    
    void transform(int dx, int dy);

    friend ostream &operator<< (ostream &out, const ColorPixel &c);
};

int pixel_distance(const Pixel &p, const Pixel &q);
int pixel_distance(const Pixel *p, const Pixel *q);
