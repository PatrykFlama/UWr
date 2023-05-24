#pragma once
#include <bits/stdc++.h>
using namespace std;


namespace calculations {
    class Rational {
        int numerator, denominator;

        int nwd(int a, int b) noexcept;

        void reduce() noexcept;


    public:
        Rational() noexcept;
        Rational(int integer) noexcept;
        Rational(int numerator = 0, int denominator = 1) noexcept;

        void repair();

        string toString() noexcept;
        string rationalToDecimal();

        /* #region //* ---setters getters--- */
        double getVal();
        int getNumerator();
        int getDenominator();

        void setNumerator(int numerator);
        void setDenominator(int denominator);
        /* #endregion */

        /* #region //* ---operator override--- */
        Rational operator+(Rational other);
        Rational operator-(Rational other);
        Rational operator*(Rational other);
        Rational operator/(Rational other);
        Rational operator-();
        Rational operator!();

        operator double();
        explicit operator int();

        // friend ostream& operator<<(ostream& out, calculations::Rational r);
        /* #endregion */
    };
}
ostream& operator<<(ostream& out, calculations::Rational r);