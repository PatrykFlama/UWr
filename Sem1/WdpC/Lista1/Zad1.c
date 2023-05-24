#include<stdio.h>

int main(){
    unsigned int n;
    scanf("%u", &n);
    if(n == 0) return 0;

    // unsigned int f1=1, f2=1;
    // while(f2 <= 2e9+50){
    //     printf("%u \n", f2);
    //     int temp = f1;
    //     f1 = f2;
    //     f2 += temp;
    // }
    
    unsigned int f1 = 1134903170, f2 = 1836311903;
    while(n < f2){
        unsigned int temp = f2;
        f2 = f1;
        f1 = temp - f1;
    }
    while(n > 0){
        if(n >= f2){
            n -= f2;
            printf("1");
        } else{
            printf("0");
        }
        unsigned int temp = f2;
        f2 = f1;
        f1 = temp - f1;
    }
    while(f1 >= 1){
        printf("0");
        unsigned int temp = f2;
        f2 = f1;
        f1 = temp - f1;
    }
}