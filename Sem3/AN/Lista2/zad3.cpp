#include <bits\stdc++.h>
using namespace std;


class Ulamek{
public:
    int licznik;
    int mianownik;

    void skroc(){        
        int a = licznik;
        int b = mianownik;
        int c;
        while (b != 0){
            c = a % b;
            a = b;
            b = c;
        }
        licznik /= a;
        mianownik /= a;
    }

    Ulamek(int l = 1, int m = 1){
        licznik = l;
        mianownik = m;
        skroc();
    }

    Ulamek operator+(Ulamek u){
        Ulamek wynik;
        wynik.licznik = licznik * u.mianownik + u.licznik * mianownik;
        wynik.mianownik = mianownik * u.mianownik;
        wynik.skroc();
        return wynik;
    }

    Ulamek operator-(Ulamek u){
        Ulamek wynik;
        wynik.licznik = licznik * u.mianownik - u.licznik * mianownik;
        wynik.mianownik = mianownik * u.mianownik;
        wynik.skroc();
        return wynik;
    }

    Ulamek operator*(Ulamek u){
        Ulamek wynik;
        wynik.licznik = licznik * u.licznik;
        wynik.mianownik = mianownik * u.mianownik;
        wynik.skroc();
        return wynik;
    }

    Ulamek operator/(Ulamek u){
        Ulamek wynik;
        wynik.licznik = licznik * u.mianownik;
        wynik.mianownik = mianownik * u.licznik;
        wynik.skroc();
        return wynik;
    }

    void wypisz_mieszane(){
        cout << licznik/mianownik << ' ' << licznik%mianownik << '/' << mianownik;
    }

    friend ostream& operator<<(ostream& os, const Ulamek& u){
        os << u.licznik << "/" << u.mianownik;
        return os;
    }
};

Ulamek pow_ulamek(int wykladnik){
    Ulamek wynik;
    
    if(wykladnik < 0) wynik.mianownik = pow(2, -wykladnik);
    else wynik.licznik = pow(2, wykladnik);

    return wynik;
}


void wypisz_z_cecha(int cecha){
    int i = 0b10000;
    Ulamek cecha_ulamek = pow_ulamek(cecha);

    cout << "cecha " << cecha << '\n';
    while (i < 0b100000) {
        cout << Ulamek(i, pow(2, 5)) * cecha_ulamek << '\n';
        i = i + 1;
    }

    cout << '\n';
}


int main() {    
    for (int i : {-1, 0, 1, }) {
        wypisz_z_cecha(i);
    }
}
