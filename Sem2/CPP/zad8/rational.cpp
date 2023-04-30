#include <bits/stdc++.h>
using namespace std;

namespace calculations {
    class Rational {
        int numerator, denominator;

        int nwd(int a, int b) {
            if (b == 0) return a;
            return nwd(b, a % b);
        }

        void reduce() {
            int n = nwd(numerator, denominator);
            numerator /= n;
            denominator /= n;
        }

    public:
        Rational() {
            numerator = 0;
            denominator = 1;
        }
        Rational(int numerator = 0, int denominator = 1) {
            this->numerator = numerator;
            this->denominator = denominator;
            repair();
        }

        void repair() {
            reduce();
            if (denominator < 0) {
                denominator *= -1;
                numerator *= -1;
            }
        }

        string toString() {
            return to_string(numerator) + "/" + to_string(denominator);
        }

        /* #region //* ---setters getters--- */
        int getNumerator() {
            return numerator;
        }
        int getDenominator() {
            return denominator;
        }

        void setNumerator(int numerator) {
            this->numerator = numerator;
            repair();
        }
        void setDenominator(int denominator) {
            this->denominator = denominator;
            repair();
        }
        /* #endregion */

        /* #region //* ---operator override--- */
        Rational operator+(Rational other) {
            int n = numerator * other.denominator + other.numerator * denominator;
            int d = denominator * other.denominator;
            return Rational(n, d);
        }
        Rational operator-(Rational other) {
            int n = numerator * other.denominator - other.numerator * denominator;
            int d = denominator * other.denominator;
            return Rational(n, d);
        }
        Rational operator*(Rational other) {
            int n = numerator * other.numerator;
            int d = denominator * other.denominator;
            return Rational(n, d);
        }
        Rational operator/(Rational other) {
            int n = numerator * other.denominator;
            int d = denominator * other.numerator;
            return Rational(n, d);
        }
        Rational operator-(){
            return Rational(-numerator, denominator);
        }
        Rational operator!(){
            if(numerator < 0) throw exception();
            return Rational(denominator, numerator);
        }

        operator double() {
            return (double)numerator/(double)denominator;
        }
        explicit operator int() {
            return numerator/denominator;
        }
        /* #endregion */
    };
}
