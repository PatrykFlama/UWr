#include <bits/stdc++.h>
using namespace std;

typedef float color_value;


class Color{
    color_value _r, _g, _b;

    inline bool out_of_range(color_value col){
        return (col < 0 || col > 255);
    }

public:
    Color(color_value r = 0, color_value g = 0, color_value b = 0){
        if(out_of_range(r) || out_of_range(g) || out_of_range(b))
            throw exception("Color value out of range");
        
        _r = r; 
        _g = g; 
        _b = b;
    }

    color_value get_r(){ return _r; }
    color_value get_g(){ return _g; }
    color_value get_b(){ return _b; }

    void set_r(color_value r){ 
        if(out_of_range(r)) throw exception("Color value out of range");
        _r = r; 
    }
    void set_g(color_value g){ 
        if(out_of_range(g)) throw exception("Color value out of range");
        _g = g; 
    }
    void set_b(color_value b){ 
        if(out_of_range(b)) throw exception("Color value out of range");
        _b = b; 
    }

    void lighten(color_value diff){
        _r = min(255, _r+diff);
        _g = min(255, _g+diff);
        _b = min(255, _b+diff);
    }

    void darken(color_value diff){
        _r = max(0, _r-diff);
        _g = max(0, _g-diff);
        _b = max(0, _b-diff);
    }

    static color_value combine(color_value a, color_value b){
        return (a+b)/2;
    }
};

class TransparentColor : Color{
    color_value _alpha;

public:
    TransparentColor(color_value r = 0, color_value g = 0, color_value b = 0, color_value alpha = 255) : Color(r, g, b){
        if(out_of_range(alpha)) throw exception("Alpha channel value out of range");
        _alpha = alpha;
    }

    color_value get_transparency(){ return _alpha; }
    void set_transparency(color_value alpha){
        if(out_of_range(alpha)) throw exception("Alpha channel value out of range");
        _alpha = alpha;
    }
};

class NamedColor : Color{
    // TODO
};


int main(){

}
