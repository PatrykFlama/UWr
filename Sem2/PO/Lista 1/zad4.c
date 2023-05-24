/*
Patryk Flama lista 1 zadanie 4
kompilacja: gcc zad4.c
*/

#include <stdio.h>
#define max(a, b) (a > b ? a : b)
#define min(a, b) (a < b ? a : b)
#define abs(a) (a >= 0 ? a : -a)

int policz_precyzje(float f, int *precyzja_przed, int *precyzja_po){
    int dec = 1, cnt = 0;
    while(dec < f) dec *= 10, cnt++;
    *precyzja_przed = cnt;
}

void tabliczka(float x_od, float x_do, float y_od, float y_do, float skok){
    if(x_od > x_do || y_od > y_do) return;

    int precyzja_przed = 1;
    int precyzja_za = 2;
    float maxx = max(abs(x_od), abs(x_do)) * max(abs(y_od), abs(y_do));
    policz_precyzje(maxx, &precyzja_przed, &precyzja_za);
    int precyzja_ogolna = precyzja_przed + precyzja_za + 1;
    
    for(float i = 0; i < (precyzja_przed+precyzja_za+3); i++) putchar(' ');
    for(float y = y_od; y < y_do; y += skok) printf("%*.*f ", precyzja_ogolna, precyzja_za, y);
    putchar('\n');

    for(float x = x_od; x < x_do; x += skok){
        printf("%*.*f: ", precyzja_ogolna, precyzja_za, x);
        for(float y = y_od; y < y_do; y += skok){
            printf("%*.*f ", precyzja_ogolna, precyzja_za, y*x);
        }
        putchar('\n');
    }
}


int main(){
    tabliczka(1.2, 2.3, 5.2, 6.14, 0.3);
    tabliczka(0.2, 1.3, 0.2, 3, 0.3);
    tabliczka(1, 30, 20, 100.14, 9);
}