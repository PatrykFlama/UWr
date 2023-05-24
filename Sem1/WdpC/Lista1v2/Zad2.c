// Patryk Flama zadanie 2
#include <stdio.h>

void swap(int *a, int *b){
    int tmp = *a;
    *a = *b;
    *b = tmp;
}


int czy_trojkat(int a, int b, int c){
    if(a <= 0 || b <= 0 || c <= 0) return 0;        //  czy isnieja ujemne => nie trojkat
    if(a > b && a > c){                 // znjadz max i sprawdz czy jest > od sumy pozostalych
        if(a < b+c) return 1;
    } else if(b > a && b > c){
        if(b < a+c) return 1;
    } else{
        if(c < a+b) return 1;
    }

    return 0;
}

int czy_prostokatny(int a, int b, int c){
    if(a > b && a > c){             // znajdz max i sprawdz czy jest > od sumy kwadratow ppozostalych
        if(a*a == b*b + c*c) return 1;
    } else if(b > a && b > c){
        if(b*b == a*a + c*c) return 1;
    } else{
        if(c*c == a*a + b*b) return 1;
    }

    return 0;
}

void znajdz_przyprosokatne(int a){
    for(int i = 1; i <= a; i++){
        for(int j = 1; j <=a; j++){
            if(a*a == i*i + j*j){
                printf("Dlugosci przyprostokatnych dla przeciwprostokatnej a = %d, to: %d %d\n", a, i, j);
                return;
            }
        }
    }

    printf("Dla przeciwprostokatnej a = %d nie istnieja przyprostokatne :(\n", a);
}

int main(){
    int a, b, c;
    printf("Podaj a, b, c: ");

    /* ------ SWAP W c -----
    scanf("%d%d%d", &a, &b, &c);                                                                                                 // normalnie posortowalbym te punkty, ale bez wbudowanych komend (typu sort lub swap (oraz bez dzialajacych wskaznikow w funkcjach)) nie robie tego 
    swap(&a, &b);
    printf("%d %d\n", a, b);
    */

    if(czy_trojkat(a, b, c) == 0){
        printf("Liczby nie tworza trojkata\n");
        znajdz_przyprosokatne(a);
    } else{
        printf("Liczby twarza trojkat\n");
        if(czy_prostokatny(a, b, c) == 1){
            printf("Trojkat jest prostokatny\n");
            return 0;
        }
        printf("Trojkat nie jest prostokatny\n");
    }
}