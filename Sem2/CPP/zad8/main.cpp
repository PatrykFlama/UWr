#include "rational.hpp"


int main(){
    calculations::Rational r1(2359348, 99900);
    calculations::Rational r2(1, 10000);
    calculations::Rational r3(1, 3);
    calculations::Rational r4(1, 2);
    calculations::Rational r5(1, 4);
    calculations::Rational r6(2, 5);
    calculations::Rational r7(r1);

    cout << "initial: \n";
    cout << r1 << '\n';
    cout << r2 << '\n';
    cout << r3 << '\n';
    cout << r4 << '\n';
    cout << r5 << '\n';
    cout << r6 << '\n';
    cout << r7 << '\n';

    cout << "operations: \n";
    cout << r1 + r6 << '\n';
    cout << r1 - r6 << '\n';
    cout << r1 * r6 << '\n';
    cout << r1 / r6 << '\n';
    cout << -r1 << '\n';
    cout << (ceil((!r1).getVal() * 10000.0) / 10000.0) << '\n';

    cout << "double and int: \n";
    cout << (double)r1 << '\n';
    cout << (int)r1 << '\n';

    cout << "copy: \n";
    r1 = r2;
    cout << r1 << '\n';
    cout << r2 << '\n';

    cout << "move: \n";
    r1 = move(r3);
    cout << r1 << '\n';
    cout << r3 << '\n';
}
