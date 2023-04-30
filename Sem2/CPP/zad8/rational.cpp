#include <bits/stdc++.h>
using namespace std;

namespace exceptions {
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

namespace calculations {
    class Rational {
        int numerator, denominator;

        int nwd(int a, int b) noexcept {
            if (b == 0) return a;
            return nwd(b, a % b);
        }

        void reduce() noexcept {
            int n = nwd(numerator, denominator);
            numerator /= n;
            denominator /= n;
        }

    public:
        Rational() noexcept {
            numerator = 0;
            denominator = 1;
        }
        Rational(int numerator = 0, int denominator = 1) noexcept {
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

         string toString() noexcept {
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
            // TODO: check if n/d is in range
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
        Rational operator-() {
            return Rational(-numerator, denominator);
        }
        Rational operator!() {
            if(numerator < 0) throw exceptions::division_by_zero();
            return Rational(denominator, numerator);
        }

        operator double() {
            return (double)numerator/(double)denominator;
        }
        explicit operator int() {
            return numerator/denominator;
        }

        friend ostream& operator<<(ostream& out, Rational r) {
            out << r.toString();
            return out;
        }
        /* #endregion */
    };
}
