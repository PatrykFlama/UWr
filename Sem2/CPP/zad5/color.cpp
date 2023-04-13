#include "color.hpp"


Color::Color(color_value r = 0, color_value g = 0, color_value b = 0){
    if(out_of_range(r) || out_of_range(g) || out_of_range(b))
        throw invalid_argument("Color value out of range");
    
    _r = r;
    _g = g;
    _b = b;
}

color_value Color::get_r() const { return _r; }
color_value Color::get_g() const { return _g; }
color_value Color::get_b() const { return _b; }

void Color::set_r(color_value r){ 
    if(out_of_range(r)) throw invalid_argument("Color value out of range");
    _r = r; 
}
void Color::set_g(color_value g){ 
    if(out_of_range(g)) throw invalid_argument("Color value out of range");
    _g = g; 
}
void Color::set_b(color_value b){ 
    if(out_of_range(b)) throw invalid_argument("Color value out of range");
    _b = b; 
}

void Color::lighten(color_value diff){
    _r = min(color_value(255), _r+diff);
    _g = min(color_value(255), _g+diff);
    _b = min(color_value(255), _b+diff);
}

void Color::darken(color_value diff){
    _r = max(color_value(0),   _r-diff);
    _g = max(color_value(0),   _g-diff);
    _b = max(color_value(0),   _b-diff);
}

color_value Color::combine(color_value a, color_value b) {
    return (a+b)/2;
}

ostream &operator<< (ostream &out, const Color &c){
    return out << c.get_r() << ' ' << c.get_g() << ' ' << c.get_b();
}

TransparentColor::TransparentColor(color_value r = 0, color_value g = 0, color_value b = 0, color_value alpha = 255) : Color(r, g, b){
    if(out_of_range(alpha)) throw invalid_argument("Alpha channel value out of range");
    _alpha = alpha;
}

color_value TransparentColor::get_transparency() const { return _alpha; }
void TransparentColor::set_transparency(color_value alpha){
    if(out_of_range(alpha)) throw invalid_argument("Alpha channel value out of range");
    _alpha = alpha;
}

ostream &operator<< (ostream &out, const TransparentColor &c){
    return out << c.get_r() << ' ' << c.get_g() << ' ' << c.get_b() << ' ' << c.get_transparency();
}

bool NamedColor::check_name(string &name) const {
    for(auto i : name)
        if(i > 'z' || i < 'a') return true;
    return false;
}

NamedColor::NamedColor(color_value r = 0, color_value g = 0, color_value b = 0, string name = "") : Color(r, g, b){
    if(check_name(name)) throw invalid_argument("Invalid name (should consist only of small letters)");
    _name = name;
}

string NamedColor::get_name() const { return _name; }
void NamedColor::set_name(string name){
    if(check_name(name)) throw invalid_argument("Invalid name (should consist only of small letters)");
    _name = name;
}

ostream &operator<< (ostream &out, const NamedColor &c){
    return out << c.get_r() << ' ' << c.get_g() << ' ' << c.get_b() << ' ' << c.get_name();
}

TransNamColor::TransNamColor(color_value r = 0, color_value g = 0, color_value b = 0,
                color_value alpha = 255, string name = "") : Color(r, g, b){
    if(out_of_range(alpha)) throw invalid_argument("Alpha channel value out of range");
    _alpha = alpha;
    if(check_name(name)) throw invalid_argument("Invalid name (should consist only of small letters)");
    _name = name;
}

ostream &operator<< (ostream &out, const TransNamColor &c){
    return out << c.get_r() << ' ' << c.get_g() << ' ' << c.get_b() << ' ' << c.get_transparency() << ' ' << c.get_name();
}
