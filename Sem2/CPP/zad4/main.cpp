// ADD pragma once later to class
#include <bits/stdc++.h>
using namespace std;


class tab_bit {
    typedef uint64_t slowo; // komorka w tablicy
    static const int rozmiarSlowa; // rozmiar slowa w bitach
    friend istream &operator>> (istream &we, tab_bit &tb);
    friend ostream &operator<< (ostream &wy, const tab_bit &tb);
    class ref; // klasa pomocnicza do adresowania bitów
protected:
    int dl; // liczba bitów
    slowo *tab; // tablica bitów
public:
    explicit tab_bit (int rozm);    // wyzerowana tablica bitow [0...rozm]
    explicit tab_bit (slowo tb);    // tablica bitów [0...rozmiarSlowa]
                                    // zainicjalizowana wzorcem
    tab_bit (const tab_bit &tb);    // konstruktor kopiujący
    tab_bit (tab_bit &&tb);     // konstruktor przenoszący
    tab_bit & operator = (const tab_bit &tb); // przypisanie kopiujące
    tab_bit & operator = (tab_bit &&tb); // przypisanie przenoszące
    ~tab_bit (); // destruktor
private:
    bool czytaj (int i) const; // metoda pomocnicza do odczytu bitu
    bool pisz (int i, bool b); // metoda pomocnicza do zapisu bitu
    public:
    bool operator[] (int i) const; // indeksowanie dla stałych tablic bitowych
    ref operator[] (int i); // indeksowanie dla zwykłych tablic bitowych
    inline int rozmiar () const; // rozmiar tablicy w bitach
public:
    // operatory bitowe: | i |=, & i &=, ^ i ^= oraz !
public:
    // zaprzyjaźnione operatory strumieniowe: << i >>
};

int main(){
    tab_bit t(46); // tablica 46-bitowa (zainicjalizowana zerami)
    tab_bit u(45ull); // tablica 64-bitowa (sizeof(uint64_t)*8)
    tab_bit v(t); // tablica 46-bitowa (skopiowana z t)
    tab_bit w(tab_bit(8){1, 0, 1, 1, 0, 0, 0, 1}); // tablica 8-bitowa (przeniesiona)
    v[0] = 1; // ustawienie bitu 0-go bitu na 1
    t[45] = true; // ustawienie bitu 45-go bitu na 1
    bool b = v[1]; // odczytanie bitu 1-go
    u[45] = u[46] = u[63]; // przepisanie bitu 63-go do bitow 45-go i 46-go
    cout<<t<<endl; // wysietlenie zawartości tablicy bitów na ekranie
}
