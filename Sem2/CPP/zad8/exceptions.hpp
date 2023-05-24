#pragma once
#include <bits/stdc++.h>   
using namespace std;


namespace exceptions {
    class rational_exception : public logic_error {
    public:
        rational_exception(const string& what) : logic_error(what){};
    };

    class division_by_zero : public rational_exception {
    public:
        division_by_zero(const string& what) : rational_exception(what){};
        const char* what() const throw() {
            return "Division by 0!";
        }
    };
    class out_of_range : public rational_exception {
    public:
        out_of_range(const string& what) : rational_exception(what){};
        const char* what() const throw() {
            return "Out of range!";
        }
    };
}
