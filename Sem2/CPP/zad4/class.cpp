#include "class.hpp"



class tab_bit::ref { // klasa pomocnicza do adresowania bitów
    word *rtab;
    int rptr;

public:
    ref(word *_tab, int _ptr){
        rtab = _tab;
        rptr = _ptr;
    }

    ~ref(){}

    int get_bit_state() const {
        word bit = (1 << rptr);
        return (*rtab)&bit;
    }

    ref &operator=(const bool b){
        word bit = (1 << rptr);
        if(b) (*rtab) |= bit;     // turn bit on
        else  (*rtab) &= ~bit;
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
};

/* #region //* constructors, assignment */
tab_bit::tab_bit (int size) {    // zeroed bits arry [0...size]
    bits_amt = size;
    tab = new word[cells()];
    for(int i = 0; i < cells(); i++){
        tab[i] = 0;
    }
}
tab_bit::tab_bit (word tb) {    // bits array [0...wordSize] initiated with pattern
    bits_amt = bitsInWord;
    tab = new word[1];
    tab[0] = tb;
}

tab_bit::tab_bit(initializer_list<bool> input) : tab_bit((int)input.size()){
    int i = 0;
    for(auto it : input){
        write_bit(i, it);
        i++;
    }
}


tab_bit::tab_bit (const tab_bit &tb) {
    bits_amt = tb.bits_amt;
    for(int i = 0; i < cells(); i++) 
        tab[i] = tb.tab[i];
}
tab_bit::tab_bit (tab_bit &&tb) {
    bits_amt = tb.bits_amt;
    tab = tb.tab;
    tb.tab = nullptr;
}
tab_bit &tab_bit::operator=(const tab_bit &tb) {
    bits_amt = tb.bits_amt;
    tab = new word[cells()];
    for(int i = 0; i < cells(); i++) 
        tab[i] = tb.tab[i];
}
tab_bit &tab_bit::operator=(tab_bit &&tb) {
    bits_amt = tb.bits_amt;
    tab = tb.tab;
    tb.tab = nullptr;
}

tab_bit::~tab_bit (){
    if(tab != nullptr) delete[] tab;
}

void tab_bit::resize(int s = -1){
    if (s > 0) bits_amt = s;

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
int tab_bit::cells() const {        // how many cells are needed/used per {amount of bits = [1, ...]} -> [1, ...]
    return ((bits_amt+1)/bitsInWord)+1;
}
int tab_bit::get_cell(int bit) const {  // get in which cell is given {bit = [0, ...]} -> [0, ...]
    return (bit/bitsInWord);
}
bool tab_bit::read_bit (int i) const{ // metoda pomocnicza do odczytu bitu
    int cell = get_cell(i);
    word bit = (1 << (i%bitsInWord));
    return (tab[cell] & bit);
}
bool tab_bit::write_bit (int i, bool b){ // metoda pomocnicza do zapisu bitu
    int cell = get_cell(i);
    word bit = (1 << (i%bitsInWord));
    if(b) tab[cell] |= bit;     // turn bit on
    else  tab[cell] &= ~bit;
}
/* #endregion */

/* #region //* user functions */
// bool tab_bit::operator[] (int i) const{ // indeksowanie dla stałych tablic bitowych
//     return read_bit(i);
// }
tab_bit::ref tab_bit::operator[] (word i){ // indeksowanie dla zwykłych tablic bitowych
    return ref(tab, (int)i);
}
int tab_bit::rozmiar () const{ // rozmiar tablicy w bitach
    return bitsInWord * cells();
}
/* #endregion */

/* #region //* operator overloads */
tab_bit & tab_bit::operator|=(const tab_bit& a){
    int cells_a = a.cells();

    if(cells() < cells_a){
        bits_amt = a.bits_amt;
        resize();
    }
    for(int i = 0; i < cells_a; i++)
        tab[i] |= a.tab[i];
    return *this;
}

tab_bit operator|(tab_bit a, const tab_bit& b){
    a |= b;
    return a;
}

tab_bit& tab_bit::operator&=(const tab_bit& a){
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

tab_bit operator&(tab_bit a, const tab_bit& b){
    a &= b;
    return a;
}

tab_bit& tab_bit::operator^=(const tab_bit& a){
    int cells_a = a.cells();

    if(cells() < cells_a){
        bits_amt = a.bits_amt;
        resize();
    }
    for(int i = 0; i < cells_a; i++)
        tab[i] ^= a.tab[i];
    return *this;
}

tab_bit operator^(tab_bit a, const tab_bit& b){
    a ^= b;
    return a;
}

tab_bit operator!(const tab_bit& a){
    tab_bit temp(a);
    for(int i = 0; i < a.cells(); i++)
        temp.tab[i] = ~temp.tab[i];
    return temp;
}
/* #endregion */

/* #region //* stream operators */
// zaprzyjaźnione operatory strumieniowe: << i >>
istream &operator>> (istream &in, tab_bit &tb){
    tb.read_bit(0);
    return in;
}
ostream &operator<< (ostream &out, const tab_bit &tb){
    string res = "";
    for(int i = 0; i < tb.cells(); i++)
        res = bitset<tb.bitsInWord>(tb.tab[i]).to_string() + res;
    return out << res;
}
/* #endregion */
