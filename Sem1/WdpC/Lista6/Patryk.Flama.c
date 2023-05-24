// Patryk Flama Lista6 Patryk.Flama

#include "operacje.h"
#define L 1000

int main(){
    char *string = (char*)malloc(L*(sizeof(char)));
    
    while(*string != '\0'){
        char *end = string; 
        scanf("%c", end);
        while(*end != '\n' && *end != ' '){
            end++;
            *end = getchar();
        }
        *(end) = '\0';

        if(czy_palindrom(string)){
            printf("PALINDROM: %s", string);
        } else{
            printf("REWERS: ");
            wypisz_rewers(string);
        }
        printf("\n");
    }

    free(string);
}
