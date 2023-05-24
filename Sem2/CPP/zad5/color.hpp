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
    Color();
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
    TransparentColor();
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
