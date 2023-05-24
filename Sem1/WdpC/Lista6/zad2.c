#include "../template.c"
#define L 101
#define N 101
const int struct_memsize = 2*L*(sizeof(char)) + 2*(sizeof(int));


typedef struct ksiazka{
    char autor[L];
    char tytul[L];
    int rok_wydania;
    int liczba_stron;
} ksiazka;


int compare_a(const void *k1, const void *k2){
    return strcmp(((ksiazka*)k1)->autor, ((ksiazka*)k2)->autor);
}
int compare_t(const void *k1, const void *k2){
    return strcmp(((ksiazka*)k1)->tytul, ((ksiazka*)k2)->tytul);
}
int compare_r(const void *k1, const void *k2){
    return (((ksiazka*)k1)->rok_wydania) - (((ksiazka*)k2)->rok_wydania);
}


int main(int argc, char** argv){
    if(argc < 3) return 0;

    FILE *input_file = fopen(argv[1], "r");
    if(input_file == NULL){
        printf("ERROR: file doesn't exist!\n");
        return 0;
    }

    ksiazka *ksiazki = (ksiazka*)malloc(N * sizeof(ksiazka));

    char s[101]; 
    int size = 0, ptr;
    while(fscanf(input_file, "%100s", &s) != EOF){
        strcpy((ksiazki+size)->autor, s);

        fscanf(input_file, "%100s", &s);
        strcpy((ksiazki+size)->tytul, s);

        fscanf(input_file, "%d", &ptr);
        (ksiazki+size)->liczba_stron = ptr;

        fscanf(input_file, "%d", &ptr);
        (ksiazki+size)->rok_wydania = ptr;

        size++;
    }

    fclose(input_file);


    // printf("-----PRZED-----\n");
    // FOR(size){
    //     printf("%s ", (ksiazki+i)->autor);
    //     printf("%s ", (ksiazki+i)->tytul);
    //     printf("%d ", (ksiazki+i)->liczba_stron);
    //     printf("%d\n", (ksiazki+i)->rok_wydania);
    // }
    // printf("---------------\n");

    if(argv[2][0] == 'a') qsort(ksiazki, size, sizeof(ksiazka), compare_a);
    else if(argv[2][0] == 't') qsort(ksiazki, size, sizeof(ksiazka), compare_t);
    else qsort(ksiazki, size, sizeof(ksiazka), compare_r);

    // printf("-----PO-----\n");
    FOR(size){
        printf("%s ", (ksiazki+i)->autor);
        printf("%s ", (ksiazki+i)->tytul);
        printf("%d ", (ksiazki+i)->liczba_stron);
        printf("%d\n", (ksiazki+i)->rok_wydania);
    }
    free(ksiazki);
    // printf("-------------\n");
}
