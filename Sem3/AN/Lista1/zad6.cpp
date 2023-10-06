#include <bits/stdc++.h>
using namespace std;
#define precision double


precision calc_pi(int n){
    precision pi = 0;
    for(int k = 0; k < n; k++){
        if(k%2) pi -= 1./(precision)(2*k+1);
        else pi += 1./(precision)(2*k+1);
    }

    return 4.*pi;
}


int main(){
    cout << setprecision(15) << calc_pi(2e6) << '\n';
    // na floatach nie działa przy 2e6 - na 6 miejscu się nie zgadza
    // wychodzi na to, że niska precyzja floata w trakcie obliczeń (brak małej części liczby/jej końcówki
    // nie może wpłynąć na wynik ostateczny
}
