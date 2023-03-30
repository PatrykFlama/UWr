// TODO ADD pragma once later to class
#include <bits/stdc++.h>
using namespace std;


class tab_bit {
    typedef uint64_t word; // komorka w tablicy
    static const int bitsInWord = (sizeof(word)*8); // rozmiar slowa w bitach

    class ref { // klasa pomocnicza do adresowania bitów
        word *rtab;
        int rptr;

    public:
        ref(word *_tab, int _ptr){
            rtab = _tab;
            rptr = _ptr;
        }

        int get_bit_state() const {
            word bit = (1 << rptr);
            return (*rtab)&bit;
        }

        ref &operator=(const int b){
            word bit = (1 << rptr);
            if(b) (*rtab) |= bit;     // turn bit on
            else  (*rtab) &= ~bit;
        }

        ref &operator=(const ref &r){
            word bit = (1 << rptr);

            int b = r.get_bit_state();

            if(b) (*rtab) |= bit;     // turn bit on
            else  (*rtab) &= ~bit;
        }

        operator bool() const{
            return get_bit_state();
        }
        // bool &operator=(const ref &r){
        //     return (bool)(r.get_bit_state()); //TODO
        // }
    };

/* #region //* VARS */
protected:
    int bits_amt;     // amount of bits
    word *tab; // bits array
/* #endregion */

/* #region //* constructors, assignment */
public:
    explicit tab_bit (int size) {    // zeroed bits arry [0...size]
        bits_amt = size;
        tab = new word[cells()];
        for(int i = 0; i < cells(); i++){
            tab[i] = 0;
        }
    }
    explicit tab_bit (word tb) {    // bits array [0...wordSize] initiated with pattern
        bits_amt = bitsInWord;
        tab = new word[1];
        tab[0] = tb;
    }

    tab_bit(initializer_list<bool> input) : bits_amt(input.size()){
        tab = new word[cells()];
        
        int i = 0;
        for(auto it : input){
            write_bit(i, it);
            i++;
        }
    }


    tab_bit (const tab_bit &tb) {
        bits_amt = tb.bits_amt;
        for(int i = 0; i < cells(); i++) 
            tab[i] = tb.tab[i];
    }
    tab_bit (tab_bit &&tb) {
        bits_amt = tb.bits_amt;
        tab = tb.tab;
        tb.tab = nullptr;
    }
    tab_bit &operator=(const tab_bit &tb) {
        bits_amt = tb.bits_amt;
        tab = new word[cells()];
        for(int i = 0; i < cells(); i++) 
            tab[i] = tb.tab[i];
    }
    tab_bit &operator=(tab_bit &&tb) {
        bits_amt = tb.bits_amt;
        tab = tb.tab;
        tb.tab = nullptr;
    }

    ~tab_bit (){
        if(tab != nullptr) delete[] tab;
    }

    void resize(){
        int new_size = cells();
        word *new_tab = new word[new_size];
        for(int i = 0; i < cells(); i++){
            new_tab[i] = tab[i];
        }
        for(int i = cells(); i < new_size; i++){
            new_tab[i] = 0;
        }
        delete[] tab;
        tab = new_tab;
    }
/* #endregion */

/* #region //* helper functions */
private:
    int cells() const {        // how many cells are needed/used per {amount of bits = [1, ...]} -> [1, ...]
        return ((bits_amt+1)/bitsInWord)+1;
    }
    int get_cell(int bit) const {  // get in which cell is given {bit = [0, ...]} -> [0, ...]
        return (bit/bitsInWord);
    }
    bool read_bit (int i) const{ // metoda pomocnicza do odczytu bitu
        int cell = get_cell(i);
        word bit = (1 << (i%bitsInWord));
        return (tab[cell] & bit);
    }
    bool write_bit (int i, bool b){ // metoda pomocnicza do zapisu bitu
        int cell = get_cell(i);
        word bit = (1 << (i%bitsInWord));
        if(b) tab[cell] |= bit;     // turn bit on
        else  tab[cell] &= ~bit;
    }
/* #endregion */

/* #region //* user functions */
public:
    bool operator[] (int i) const{ // indeksowanie dla stałych tablic bitowych
        return read_bit(i);
    }
    ref operator[] (int i){ // indeksowanie dla zwykłych tablic bitowych
        return ref(tab, i);
    }
    inline int rozmiar () const{ // rozmiar tablicy w bitach
        return bitsInWord * cells();
    }
/* #endregion */

/* #region //* operator overloads */
public:
    tab_bit& operator|=(const tab_bit& a){
        int cells_a = a.cells();

        if(cells() < cells_a){
            bits_amt = a.bits_amt;
            resize();
        }
        for(int i = 0; i < cells_a; i++)
            tab[i] |= a.tab[i];
        return *this;
    }

    friend tab_bit operator|(tab_bit a, const tab_bit& b){
        a |= b;
        return a;
    }
    
    tab_bit& operator&=(const tab_bit& a){
        int cells_a = a.cells();

        if(cells() < cells_a){
            bits_amt = a.bits_amt;
            resize();
        }
        for(int i = 0; i < cells_a; i++)
            tab[i] &= a.tab[i];
        for(int i = cells_a; i < cells(); i++)
            tab[i] = 0;
        return *this;
    }

    friend tab_bit operator&(tab_bit a, const tab_bit& b){
        a &= b;
        return a;
    }

    tab_bit& operator^=(const tab_bit& a){
        int cells_a = a.cells();

        if(cells() < cells_a){
            bits_amt = a.bits_amt;
            resize();
        }
        for(int i = 0; i < cells_a; i++)
            tab[i] ^= a.tab[i];
        return *this;
    }

    friend tab_bit operator^(tab_bit a, const tab_bit& b){
        a ^= b;
        return a;
    }

    friend tab_bit operator!(const tab_bit& a){
        tab_bit temp(a);
        for(int i = 0; i < a.cells(); i++)
            temp.tab[i] = ~temp.tab[i];
        return temp;
    }
    /* #endregion */

/* #region //* stream operators */
public:
    // zaprzyjaźnione operatory strumieniowe: << i >>
    // friend istream &operator>> (istream &in, tab_bit &tb){
    //     return in >> tb.read_bit(0);
    // }
    friend ostream &operator<< (ostream &out, const tab_bit &tb){
        string res = "";
        for(int i = 0; i < tb.cells(); i++)
            res = bitset<tb.bitsInWord>(tb.tab[i]).to_string() + res;
        return out << res;
    }
    /* #endregion */
};

int main(){
    tab_bit t(46); // tablica 46-bitowa (zainicjalizowana zerami)
    tab_bit u(45ull); // tablica 64-bitowa (sizeof(uint64_t)*8)
    tab_bit v(t); // tablica 46-bitowa (skopiowana z t)
    tab_bit w(tab_bit(8){1, 0, 1, 1, 0, 0, 0, 1});
    v[0] = 1; // ustawienie bitu 0-go bitu na 1
    t[45] = true; // ustawienie bitu 45-go bitu na 1
    bool b = v[1];
    u[45] = u[46] = u[63]; // przepisanie bitu 63-go do bitow 45-go i 46-go
    cout<<t<<endl; // wysietlenie zawartości tablicy bitów na ekranie
}
