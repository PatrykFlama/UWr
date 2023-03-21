#include "class.hpp"


int main(){
    Number a;
    Number b(8);

    a.insert(1);
    a.insert(2);
    a.insert(3);
    b.insert(9);
    b.insert(10);

    Number c = a;

    cout << a.get_num() << ' ' << c.get_num() << ' ' << b.get_num() << '\n';
    cout << a.get_history(1) << ' ' << ' ' << b.get_history(1) << '\n';

}