// Patryk Flama zadanie 2

// #region macros
#define GET_MACRO(_1, _2, _3, _4, NAME, ...) NAME
#define _FOR3(i, a, n, inc) for (int i = (a); (inc) > 0 ? i < (n) : i >= (n); i += (inc))
#define _FOR2(i, a, n) _FOR3(i, a, n, 1)
#define _FOR1(i, n) _FOR2(i, 0, n)
#define _FOR0(n) _FOR1(i, n)
#define FOR(...) GET_MACRO(__VA_ARGS__, _FOR3, _FOR2, _FOR1, _FOR0)(__VA_ARGS__)    //? (/name/, //from//, to, ///inc///)
#define cerr if (debug) cout
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
// #endregion */

const int L = 1e6;


int NumGet(int baza){
    char c = getchar();
    int res = 0;

    while(c != '.'){
        if(!((c-'0' < baza && c >= '0') || (c >= 'A' && c <= 'F'))){c = getchar(); continue;}
        res *= baza;

        if(c >= 'A' && c <= 'F') res += c-'A'+10;
        else res += c-'0';

        c = getchar();
    }
    
    return res;
}

void NumPut(int baza, int x){
    int power = 1;
    while(power <= x){
        power *= baza;
    }
    power /= baza;

    // printf("Wyliczona potega z bazy %d do liczby %d to: %d\n", baza, x, power);
    // scanf("%d", &power);

    while(x > 0){
        char amt = '0';
        while(x >= power){
            amt++, x -= power;
        }
        // printf("\npower %d x: %d, amt: ", power, x);
        if(amt > '9')
            putchar(amt-'9'+'A'-1);
        else putchar(amt);
        power /= baza;
    }
    while(power > 0){
        power /= baza;
        putchar('0');
    }
}

int silnia(int n){
    if(n == 0) return 1;
    int res = 1;
    FOR(i, 2, n+1){
        res *= i;
    }
    return res;
}

int main(){
    char operacja;
    int baza = 16;
    int liczba = 1, liczba_pomocnicza;

    while(liczba != 0){
        liczba = NumGet(baza);
        if(liczba == 0) break;
        operacja = getchar();
        while(operacja != '+' && operacja != '-' && operacja != '*' && operacja != '/' && operacja != '!') operacja = getchar();

        int wynik;
        switch (operacja){
        case '+':
            liczba_pomocnicza = NumGet(baza);
            wynik = liczba+liczba_pomocnicza;
            NumPut(baza, wynik); printf("\n");
            break;
        case '-':
            liczba_pomocnicza = NumGet(baza);
            wynik = liczba-liczba_pomocnicza;
            NumPut(baza, wynik); printf("\n");
            break;
        case '*':
            liczba_pomocnicza = NumGet(baza);
            wynik = liczba*liczba_pomocnicza;
            NumPut(baza, wynik); printf("\n");
            break;
        case '/':
            liczba_pomocnicza = NumGet(baza);
            wynik = liczba/liczba_pomocnicza;
            NumPut(baza, wynik); printf("\n");
            break;
        case '!':
            wynik = silnia(liczba);
            NumPut(baza, wynik); printf("\n");
            break;
        
        default:
            break;
        }

    }
}
