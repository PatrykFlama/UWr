// Patryk Flama zadanie 1

#include <stdbool.h>
#include <stdio.h>
#include <string.h>

const int L = 1e6;


int NumGet(int baza){
    char c = getchar();
    int res = 0;

    while(c != '.'){
        if(c-'0' >= baza || c < '0'){c = getchar(); continue;}
        res *= baza;
        res += c-'0';
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
        putchar(amt);
        power /= baza;
    }
    while(power > 0){
        power /= baza;
        putchar('0');
    }
}

int main(){
    int baza;
    int liczba = 1;
    printf("?\n");
    scanf("%d", &baza);

    while(liczba != 0){
        printf("Podaj liczbe: ");
        liczba = NumGet(baza);
        printf("system 2: "); NumPut(2, liczba); printf("\n");
        printf("system 8: "); NumPut(8, liczba); printf("\n");
        printf("system 10: "); NumPut(10, liczba); printf("\n");
    }
}
