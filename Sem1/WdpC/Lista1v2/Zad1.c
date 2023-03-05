// Patryk Flama zadanie 1
#include <stdio.h>

float policz_pierwiastek(float x, float d){
    float l = 0, r = x, mid;
    while(r-l >= 2*d){
        mid = (l+r)/2;
        // printf("l = %f, r = %f, mid = %f\n", l, r, mid);
        if(mid*mid > x) r = mid;
        else l = mid;
    }

    return mid;
}

int main(){
    float x; // pierwiastkowana liczba
    float d; // dokladnosc
    while(1){
        printf("Podaj liczbe oraz dokladnosc: ");
        scanf("%f", &x);
        if(x == 1) return 0;
        scanf("%f", &d);
        printf("%f\n", policz_pierwiastek(x, d));
    }
}