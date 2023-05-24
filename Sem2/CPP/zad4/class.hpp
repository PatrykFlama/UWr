#pragma once
#include <bits/stdc++.h>
using namespace std;


class tab_bit {
    typedef uint64_t word; // komorka w tablicy
    static const int bitsInWord = (sizeof(word)*8); // rozmiar slowa w bitach

    class ref;

/* #region //* VARS */
protected:
    int bits_amt;     // amount of bits
    word *tab; // bits array
/* #endregion */

/* #region //* constructors, assignment */
public:
    explicit tab_bit (int size);
    explicit tab_bit (word tb);

    explicit tab_bit(initializer_list<bool> input);


    tab_bit (const tab_bit &tb);
    tab_bit (tab_bit &&tb);
    tab_bit &operator=(const tab_bit &tb);
    tab_bit &operator=(tab_bit &&tb);

    ~tab_bit ();

    void resize(int s);
/* #endregion */

/* #region //* helper functions */
private:
    int cells() const;
    int get_cell(int bit) const;
    bool read_bit (int i = 0) const;
    bool write_bit (int i, bool b);
/* #endregion */

/* #region //* user functions */
public:
    // bool operator[] (int i) const;
    ref operator[] (word i);
    int rozmiar () const;
/* #endregion */

/* #region //* operator overloads */
public:
    tab_bit& operator|=(const tab_bit& a);

    friend tab_bit operator|(tab_bit a, const tab_bit& b);
    
    tab_bit& operator&=(const tab_bit& a);

    friend tab_bit operator&(tab_bit a, const tab_bit& b);

    tab_bit& operator^=(const tab_bit& a);

    friend tab_bit operator^(tab_bit a, const tab_bit& b);

    friend tab_bit operator!(const tab_bit& a);
    /* #endregion */

/* #region //* stream operators */
public:
    // zaprzyjaźnione operatory strumieniowe: << i >>
    friend istream &operator>> (istream &in, tab_bit &tb);
    friend ostream &operator<< (ostream &out, const tab_bit &tb);
};
