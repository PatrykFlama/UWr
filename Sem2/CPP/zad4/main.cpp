// TODO ADD pragma once later to class
#include <bits/stdc++.h>
using namespace std;


class tab_bit {
    typedef uint64_t slowo; // komorka w tablicy
    static const int rozmiarSlowa; // rozmiar slowa w bitach
    class ref { // klasa pomocnicza do adresowania bitów

    };

/* #region //* VARS */
protected:
    int dl;     // amount of bits
    slowo *tab; // bits array
/* #endregion */

/* #region //* constructors, assignment */
public:
    explicit tab_bit (int rozm) {    // zeroed bits arry [0...size]
        // TODO
    }
    explicit tab_bit (slowo tb) {    // bits array [0...wordSize] initiated with pattern
        // TODO
    }
    
    tab_bit (const tab_bit &tb) {
        dl = tb.dl;
        copy(tb.tab, tb.tab+dl, tab);   // TODO is tab+dl correct or should it be smth like tab+(dl/[bits(char)=8])
    }
    tab_bit (tab_bit &&tb) {
        dl = tb.dl;
        tab = tb.tab;
        tb.tab = nullptr;
    }
    tab_bit &operator=(const tab_bit &tb) {
        // TODO but i think its the same as operator one
    }
    tab_bit &operator=(tab_bit &&tb) {
        // TODO but i think its the same as operator one
    }
    ~tab_bit (){
        if(tab != nullptr) delete[] tab;
    }
/* #endregion */

/* #region //* helper functions */
private:
    int cells() const {        // how many cells are needed/used per amount of bits
        return 0;
    }
    int get_cell(int bit) const {  // get in which cell is given bit

    }
    bool czytaj (int i) const; // metoda pomocnicza do odczytu bitu
    bool pisz (int i, bool b); // metoda pomocnicza do zapisu bitu
/* #endregion */

/* #region //* user functions */
public:
    bool operator[] (int i) const; // indeksowanie dla stałych tablic bitowych
    ref operator[] (int i); // indeksowanie dla zwykłych tablic bitowych
    inline int rozmiar () const{ // rozmiar tablicy w bitach
        return sizeof(slowo) * (cells());
    }
/* #endregion */

/* #region //* operator overloads */
public:
    tab_bit& operator|=(const tab_bit& a){
        // TODO operation
        return *this;
    }

    friend tab_bit operator|(tab_bit a, const tab_bit& b){
        a |= b;
        return a;
    }
    
    tab_bit& operator&=(const tab_bit& a){
        // TODO operation
        return *this;
    }

    friend tab_bit operator&(tab_bit a, const tab_bit& b){
        a &= b;
        return a;
    }

    tab_bit& operator^=(const tab_bit& a){
        // TODO operation
        return *this;
    }

    friend tab_bit operator^(tab_bit a, const tab_bit& b){
        a ^= b;
        return a;
    }

    friend tab_bit operator!(const tab_bit& a){
        // TODO operation
        return a;
    }
    /* #endregion */

/* #region //* stream operators */
public:
    // zaprzyjaźnione operatory strumieniowe: << i >>
    friend istream &operator>> (istream &we, tab_bit &tb);
    friend ostream &operator<< (ostream &wy, const tab_bit &tb);
    /* #endregion */
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
