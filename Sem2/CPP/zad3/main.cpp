#include "class.hpp"


int main(){
    Number a;
    Number b(5);

    b.insert(6);
    Number c(b);
    Number d;
    b.insert(7);
    d = b;

    printf("a:%f b:%f c:%f d:%f\n", a, b, c, d);
}