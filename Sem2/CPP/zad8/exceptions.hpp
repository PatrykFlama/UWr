#pragma once
#include <bits/stdc++.h>   
using namespace std;


namespace exceptions {
    class rational_exception : public logic_error {
        const char* what() const throw() {
            return "Rational exception!";
        }
    };

    class division_by_zero : public exception {
        const char* what() const throw() {
            return "Division by 0!";
        }
    };
    class out_of_range : public exception {
        const char* what() const throw() {
            return "Out of range!";
        }
    };

}
