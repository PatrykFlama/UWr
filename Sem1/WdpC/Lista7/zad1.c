#include "../template.c"


int gettime(){
    int var[3], len;
    var[0] = var[1] = var[2] = len = 0;

    char input = getchar();
    while(input == ' ' || input == '\n') input = getchar();
    
    while(input != ' ' && input != '\n'){
        if(input == ':'){
            len++;
            input = getchar();
            continue;
        }

        var[len] *= 10;
        var[len] += input - '0';
        
        input = getchar();
    }

    switch (len){
    case 0:     // only seconds
        return var[0];
        break;
    case 1:     // seconds and minutes
        return  var[0] * 60 + var[1];
        break;
    case 2:     // sec min hr
        return var[0] *60*60 + var[1] *60 + var[2];
        break;
    }
}

char get_op(){
    char in = 0;
    while(in != '+' && in != '-' && in != '*' && in != '/') in = getchar();
    return in;
}

int make_operation(int time1, int time2, char op){
    switch (op){
    case '+':
        return time1 + time2;
        break;
    case '-':
        return time1 - time2;
        break;
    case '*':
        return time1 * time2;
        break;
    case '/':
        return time1 / time2;
        break;
    }
}


void print_time(int time){
    if(time < 0){
        putchar('-');
        time = -time;
    }

    if(time >= 3600){
        printf("%d:%02d:%02d", time/3600, (time%3600)/60, time%60);
    } else if(time >= 60){
        printf("%02d:%02d", time/60, time%60);
    } else{
        printf("%02d", time);
    }
}


int main(){
    int time1 = 1, time2 = 1;
    char op;

    while(!feof(stdin)){
        time1 = gettime();
        if(time1 == 0) return 0;
        op = get_op();
        time2 = gettime();

        int res = make_operation(time1, time2, op);
        print_time(res);
        putchar('\n');
    }
}