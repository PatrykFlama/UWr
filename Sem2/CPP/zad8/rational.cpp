#include "rational.hpp"
#include "exceptions.hpp"


int calculations::Rational::nwd(int a, int b) noexcept {
    if (b == 0) return a;
    return nwd(b, a % b);
}

void calculations::Rational::reduce() noexcept {
    int n = nwd(numerator, denominator);
    numerator /= n;
    denominator /= n;
}

string calculations::Rational::rationalToDecimal() {
    // If numerator is zero, return "0"
    if (numerator == 0) return "0";
    
    // Check the sign of the result
    string result;
    if (numerator < 0) result += "-";
    
    // Convert numerator and denominator to positive
    int n = abs(numerator);
    int d = denominator;
    
    // Calculate the integer part
    result += to_string(n / d);
    if(n % d == 0) return result;

    // Calculate the decimal part (long division)
    result += ".";
    
    map<int, int> m;
    int r = n % d;
    while (r != 0 && m.find(r) == m.end()) {
        m[r] = result.length();
        r *= 10;
        result += to_string(r / d);
        r %= d;
    }
    
    if (r != 0) {
        result.insert(m[r], "(");
        result += ")";
    }
    
    return result;
}

calculations::Rational::Rational() noexcept {
    numerator = 0;
    denominator = 1;
}
calculations::Rational::Rational(int integer) noexcept : Rational(integer, 1) {}
calculations::Rational::Rational(int numerator, int denominator) noexcept {
    this->numerator = numerator;
    this->denominator = denominator;
    repair();
}

void calculations::Rational::repair() {
    reduce();
    if (denominator < 0) {
        denominator *= -1;
        numerator *= -1;
    }
}

    string calculations::Rational::toString() noexcept {
    return to_string(numerator) + "/" + to_string(denominator);
}

/* #region //* ---setters getters--- */
double calculations::Rational::getVal(){
    return (double)numerator/(double)denominator;
}
int calculations::Rational::getNumerator() {
    return numerator;
}
int calculations::Rational::getDenominator() {
    return denominator;
}

void calculations::Rational::setNumerator(int numerator) {
    this->numerator = numerator;
    repair();
}
void calculations::Rational::setDenominator(int denominator) {
    this->denominator = denominator;
    repair();
}
/* #endregion */

/* #region //* ---operator override--- */
calculations::Rational calculations::Rational::operator+(Rational other) {
    int n = numerator * other.denominator + other.numerator * denominator;
    int d = denominator * other.denominator;
    if(((long long)numerator * (long long)other.denominator + 
        (long long)other.numerator * (long long)denominator) != n) throw exceptions::out_of_range("out of range");
    if((long long)denominator * (long long)other.denominator != d) throw exceptions::out_of_range("out of range");
    return Rational(n, d);
}
calculations::Rational calculations::Rational::operator-(Rational other) {
    int n = numerator * other.denominator - other.numerator * denominator;
    int d = denominator * other.denominator;
    if(((long long)numerator * (long long)other.denominator - 
        (long long)other.numerator * (long long)denominator) != n) throw exceptions::out_of_range("out of range");
    if((long long)denominator * (long long)other.denominator != d) throw exceptions::out_of_range("out of range");
    return Rational(n, d);
}
calculations::Rational calculations::Rational::operator*(Rational other) {
    int n = numerator * other.numerator;
    int d = denominator * other.denominator;
    if((long long)numerator * (long long)other.numerator != n) throw exceptions::out_of_range("out of range");
    if((long long)denominator * (long long)other.denominator != d) throw exceptions::out_of_range("out of range");
    return Rational(n, d);
}
calculations::Rational calculations::Rational::operator/(Rational other) {
    int n = numerator * other.denominator;
    int d = denominator * other.numerator;
    if((long long)numerator * (long long)other.denominator != n) throw exceptions::out_of_range("out of range");
    if((long long)denominator * (long long)other.numerator != d) throw exceptions::out_of_range("out of range");
    return Rational(n, d);
}
calculations::Rational calculations::Rational::operator-() {
    return Rational(-numerator, denominator);
}
calculations::Rational calculations::Rational::operator!() {
    if(numerator < 0) throw exceptions::division_by_zero("div by zero");
    return Rational(denominator, numerator);
}

calculations::Rational::operator double() {
    return (double)numerator/(double)denominator;
}
calculations::Rational::operator int() {
    return numerator/denominator;
}

ostream& operator<<(ostream& out, calculations::Rational r) {
    out << r.rationalToDecimal();
    return out;
}
/* #endregion */



