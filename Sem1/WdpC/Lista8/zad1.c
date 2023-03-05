#include "../template.c"

typedef struct Zespolona{
    float real;
    float imaginary;
} Zespolona;

Zespolona add(Zespolona z1, Zespolona z2){
    z1.real += z2.real;
    z1.imaginary += z2.imaginary;
    return z1;
}

Zespolona subs(Zespolona z1, Zespolona z2){
    z1.real -= z2.real;
    z1.imaginary -= z2.imaginary;
    return z1;
}

Zespolona mult(Zespolona z1, Zespolona z2){
    Zespolona res;
    res.real = z1.real*z2.real - z1.imaginary*z2.imaginary;
    res.imaginary = z1.real*z2.imaginary + z1.imaginary*z2.real;
    return res;
}

Zespolona set(float r, float i){
    Zespolona n;
    n.real = r;
    n.imaginary = i;
    return n;
}

float square(Zespolona z){
    return z.real*z.real + z.imaginary*z.imaginary;
}

Zespolona fun(Zespolona z, Zespolona c, int loops){
    while(loops-- && (square(z) <= 1e6)){
        z = subs(mult(z, z), c);
    }

    return z;
}

void initialize_file(FILE *output_file, int h, int w){
    fprintf(output_file, "P3\n%d %d\n255\n", h+1, w+1);
}

void print(bool to_file, FILE *output_file, char what){
    if(to_file){
        if(what == '\n'){
            fprintf(output_file, " ");
        }
        else if(what == ' '){
            fprintf(output_file, "255 255 255 ");
        } else{
            fprintf(output_file, "0 0 0 ");
        }
    } else{
        printf("%c", what);
    }
}

int main(int argc, char **argv){
    int x_res = 40, y_res = 25;
    Zespolona c;
    FILE *output_file;

    if(argc > 2){
        x_res = atoi(argv[1]), y_res = atoi(argv[2]);
    } 
    if(argc > 4){
        c = set(atof(argv[3]), atof(argv[4]));
    } else{
        c = set(0.2, 0.75);
    }
    if(argc > 5){
        output_file = fopen(argv[5], "w");
        initialize_file(output_file, y_res, x_res);
    }

    // for(float y = -1; y <= 1; y += 2./y_res){
    //     for(float x = -1; x <= 1; x += 2./x_res){
    for(int y_step = 0; y_step <= y_res; y_step++){
        for(int x_step = 0; x_step <= x_res; x_step++){
            if(square(fun(set(-1 + 2./x_res * x_step, -1 + 2./y_res * y_step), c, 100)) < 4){
                print((argc > 5), output_file, 'O');
            } else{
                print((argc > 5), output_file, ' ');
            }
        }
        print((argc > 5), output_file, '\n');
    }
}
