#include "../template.c"


int main(){
    int w, h; scanf("%d%d", &w, &h);
    char *map = (char*)malloc(h*w*sizeof(char));


    FOR(r, h){
        FOR(c, w){
            *(map+r*h+c) = getchar();
            putchar(*(map+r*h+c));
        }
        // getchar();
    }

    FOR(r, h){
        FOR(c, w){
            putchar(*(map+r*h+c));
        }
        // putchar('\n');
    }
}