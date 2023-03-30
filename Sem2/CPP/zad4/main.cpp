#include "class.hpp"

int main(){
    tab_bit t(46); // tablica 46-bitowa (zainicjalizowana zerami)
    tab_bit u(45ull); // tablica 64-bitowa (sizeof(uint64_t)*8)
    tab_bit v(t); // tablica 46-bitowa (skopiowana z t)
    tab_bit w{1, 0, 1, 1, 0, 0, 0, 1};
    cout << t << '\n';
    cout << u << '\n';
    cout << v << '\n';
    cout << w << '\n';
    cout << "T\n";

    u |= t;
    cout << "u or t\n" << u << '\n';
    u &= w;
    cout << "u and w\n" << w << '\n';
    w ^= t;
    cout << "w xor t\n" << w << '\n';

    tab_bit h(2);
    cout << h.rozmiar() << '\n' << h << '\n';
    h = w;
    cout << h.rozmiar() << '\n' << h << '\n';
    h.resize(100);
    cout << h.rozmiar() << '\n';

    cout << "rozmiary\n" << w.rozmiar() << ' ' <<  t.rozmiar() << ' ' << u.rozmiar() << '\n';
    
}
