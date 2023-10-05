#include <bits/stdc++.h>
using namespace std;

double f(double x){
    return 4046*(sqrt(pow(x, 14)+1)-1)/pow(x, 14);
}

int main(){
    double x = 0.001;
    cout << "f(0.001) zwraca " << f(x) << endl;
    cout << "nie jest to wiarygodny wynik, bo x^14 szybko bedzie wymagac duzej precyzji\n"
         << "natomiast po dodaniu 1 do niego, zmniejszymy precyzje aby moc skorzystac z liczby > 1" << '\n';
    cout << "pow(x, 14)+1 = " << (double)pow(x, 14)+1 << endl;
}
