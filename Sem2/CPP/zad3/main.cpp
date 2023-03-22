#include "class.hpp"


int main(){
    Number a;
    Number b(8);

    a.insert(1);
    a.insert(2);
    a.insert(3);
    b.insert(9);
    b.insert(10);

    Number c;
    c = a;
    a.insert(4);

    printf("-------------------\n");
    printf("%lf %lf %lf\n", b.get_num(), c.get_num(), a.get_num());
    for(int i = 1; i < 4; i++){
        printf("%lf %lf %lf\n", b.get_history(i), c.get_history(i), a.get_history(i));
    }

    printf("----------move a to d---------\n");
    Number d;
    d = move(a);            //  a no longer exists
    printf("%lf %lf %lf\n", b.get_num(), c.get_num(), d.get_num());
    for(int i = 1; i < 4; i++){
        printf("%lf %lf %lf\n", b.get_history(i), c.get_history(i), d.get_history(i));
    }

    printf("----------d revert 2---------\n");
    d.revert(2);
    for(int i = 0; i < 4; i++){
        printf("%lf %lf %lf\n", b.get_history(i), c.get_history(i), d.get_history(i));
    }
}