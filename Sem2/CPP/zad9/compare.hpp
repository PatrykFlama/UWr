#pragma once
#include <bits/stdc++.h>
using namespace std;

template<class T> 
class compare_types {
    public:
        static bool leq(T x, T y) { return x <= y; }
        static bool equal(T x, T y) { return x == y; }
        static bool geq(T x, T y) { return x >= y; }
};

template <>
class compare_types<const char*> {
    public:
        static bool leq(const char* x, const char* y) { return strcmp(x, y) <= 0; }
        static bool equal(const char* x, const char* y) { return strcmp(x, y) == 0; }
        static bool geq(const char* x, const char* y) { return strcmp(x, y) >= 0; }
};

template <>
class compare_types<char*> {
    public:
        static bool leq(char* x, char* y) { return strcmp(x, y) <= 0; }
        static bool equal(char* x, char* y) { return strcmp(x, y) == 0; }
        static bool geq(char* x, char* y) { return strcmp(x, y) >= 0; }
};

template <class T>
class compare_types<T*> {
    public:
        static bool leq(T *x, T *y) { return (*x) <= (*y); }
        static bool equal(T *x, T *y) { return (*x) == (*y); }
        static bool geq(T *x, T *y) { return (*x) >= (*y); }
};

