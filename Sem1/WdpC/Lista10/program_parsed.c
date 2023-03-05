// Patryk Flama  zad1

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <stddef.h>
#include <float.h>
#include <math.h>
FILE *input;
#define R 6371000
const float PI = 3.14159265359;


typedef struct PtList{
    float h;        // height in meters
    float x, y;
    char name[50];
    PtList* next_item;
} PtList;

PtList* newElement(){
    PtList *p;
    p = (PtList*)malloc(sizeof(PtList));
    return p;
}

PtList* addNewElement(PtList *old){
    PtList *next = newElement();
    old->next_item = next;
    old->next_item->next_item = NULL;
    return old->next_item;
}

bool read_line(PtList *ptlist){     // false - end of file
    float h, x, y;
    if(fscanf(input, " %f %f %f", &h, &x, &y) != 3) return false;

    PtList *next = addNewElement(ptlist);
    (next->h) = h, (next->x) = x, (next->y) = y;

    char in = fgetc(input);
    while(in == ' ') in = fgetc(input);

    int ptr = 0;
    while(in != '\n'){
        *(next->name + ptr) = in;
        ++ptr;
        in = fgetc(input);
    }
    *(next->name + ptr) = '\0';

    return true;
}

void delete_list(PtList *element){
    if(element->next_item != NULL) element->next_item;
    free(element);
}

int get_number_of_rows(){
    int rows = 0;
    char in = ' ';
    while(!feof(input)){
        in = fgetc(input);
        if(in == '\n') rows++;
    }

    rewind(input);
    return rows;
}

void print_line(PtList *ptlist){
    printf("%f %f %f %s", ptlist->h, ptlist->x, ptlist->y, ptlist->name);
}

float dist(PtList *from, PtList *to){
    float from_x = R * cos(from->x*PI/180.0) * cos(from->y*PI/180.0);
    float from_y = R * cos(from->x*PI/180.0) * sin(from->y*PI/180.0);
    float from_z = R * sin(from->x*PI/180.0) + from->h;

    float to_x = R * cos(to->x*PI/180.0) * cos(to->y*PI/180.0);
    float to_y = R * cos(to->x*PI/180.0) * sin(to->y*PI/180.0);
    float to_z = R * sin(to->x*PI/180.0) + to->h;

    return sqrt((to_x-from_x)*(to_x-from_x) + (to_y-from_y)*(to_y-from_y) + (to_z-from_z)*(to_z-from_z));
}

int main(int argc, char **argv){
    if(argc < 2) return 0;
    input = fopen(argv[1], "r");

    int file_rows = get_number_of_rows();
    PtList *begin = (PtList*)malloc(sizeof(PtList));
    PtList *ptr = begin;
    int elements = 0;

    for(auto i = 0; i < file_rows; i++){
        if(!read_line(ptr)){
            fscanf(input, "%s", &(ptr->name));
            if(strstr(ptr->name, "KONIEC") != NULL) break;
            else{
                char c = fgetc(input);
                while(c != '\n') c = fgetc(input);
                goto read_loop;
            }
        }

        ptr = ptr->next_item;
        elements++;
        
        read_loop:;
    }

    printf("Wczytanych elementow: %d\n", file_rows);
    
    ptr = begin;
    if(argc == 2){
        while(ptr->next_item != NULL){
            print_line(ptr);
            putchar('\n');
            ptr = ptr->next_item;
        }
    } else if(argc == 3){
        while(ptr->next_item != NULL){
            if(strstr(ptr->name, argv[2]) != NULL) {
                print_line(ptr);
                putchar('\n');
            }
            ptr = ptr->next_item;
        }
    } else if(argc == 4 && strcmp(argv[3], "dist") == 0){
        PtList* first;
        while(ptr->next_item != NULL){
            if(strstr(ptr->name, argv[2]) != NULL) {
                first = ptr;
                break;
            }
            ptr = ptr->next_item;
        }

        printf("Distance from ");
        print_line(first);
        putchar('\n');
        ptr = begin;
        while(ptr->next_item != NULL){
            if(ptr == first){
                ptr = ptr->next_item;
                continue;
            }
            print_line(ptr);
            printf(" dist: %f meters\n", dist(first, ptr));
            ptr = ptr->next_item;
        }
    }

    delete_list(begin);
    fclose(input);
}
