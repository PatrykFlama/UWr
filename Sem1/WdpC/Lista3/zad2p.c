// Patryk Flama zadanie 2

// #region
/* --- LIBRARIES --- */
#include <stdbool.h>
#include <stdio.h>
#include <string.h>


const int L = 200;


int main(){
    char name[L]; int ptr = 0;
    int zganiezdzenie = 0;
    char c, last = '0';

    c = getchar();
    while(c != EOF){
        if(zganiezdzenie == 0 && (last == '\n' && c != '{')) ptr = 0;
        if(zganiezdzenie == 0){
            name[ptr++] = c;
        }

        if(c == '{'){
            Open:
            putchar('\n');
            last = '\n';

            char space = getchar();
            while((space == ' ' || space == '\t' || space == '\n')){
                space = getchar();
            }
            c = space;

            for(int i = 0; i < zganiezdzenie*4; i++) putchar(' ');

            putchar('{');
            zganiezdzenie++;
            continue;
        }
        
        if(c == '}'){
            Close:
            zganiezdzenie--;
            char space = getchar();
            while((space == ' ' || space == '\t' || space == '\n')){
                space = getchar();
            }
            c = space;
            // printf("putting %d spaces:", zganiezdzenie*4);

            for(int i = 0; i < zganiezdzenie*4; i++) putchar(' ');
            // FOR(zganiezdzenie*4){ putchar(' ');}
            putchar('}');

            if(zganiezdzenie == 0){
                printf("\t// ");
                for(int i = 0; i < ptr; i++) putchar(name[i]);
                printf(" --- THE END\n");
            }
            if(zganiezdzenie < 0){
                printf("Nieodpowienia zastosowanie nawiasoawnia!\n");
                return 0;
            }
            last = '\n';
            
            c = getchar();
            continue;
        } 
        
        if(last == '\n' && (c == ' ' || c == '\t')){
            char space = getchar();
            while((space == ' ' || space == '\t')){
                space = getchar();
            }
            c = space;
            if(space == '{') goto Open;
            if(space == '}') goto Close;

            for(int i = 0; i < zganiezdzenie*4; i++) putchar(' ');
            continue;
        }
    
        putchar(c);

        last = c;
        c = getchar();
    }

    putchar('\n');
}