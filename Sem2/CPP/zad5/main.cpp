#include <bits/stdc++.h>
using namespace std;


class Color{
    float r, g, b;

    bool out_of_range(float col){
        return (col < 0 || col > 255);
    }

public:
    Color(float _r = 0, float _g = 0, float _b = 0){
        if(out_of_range(_r) || out_of_range(_g) || out_of_range(_b)){}
    }
}


int main(){

}
