#include "../template.c"


int get_time(bool *line_ended, char input){
    int var[3], len;
    var[0] = var[1] = var[2] = len = 0;

    while(input == ' ' || input == ']' || input == ',') input = getchar();
    
    while(input != ' ' && input != ',' && input != ']'){
        if(input == '\n'){
            *line_ended = true;
            break;
        }
        if(input == ':'){
            len++;
            input = getchar();
            continue;
        }

        var[len] *= 10;
        var[len] += input - '0';
        
        input = getchar();
    }

    if(!(*line_ended)){
        while(input != ' ' && input != ',' && input != '\n') input = getchar();
        if(input == '\n') *line_ended = true;
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
    char in = getchar();
    while(in == ' ' || in == ']') in = getchar();
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


typedef struct TimeRange{
    int from, to;
} TimeRange;

TimeRange get_time_range(bool *line_ended){           // no time range == 1 element time range
    //TODO: find first non ' ' char and if brackets then calc begin, end; else set 1 element range
    char input = getchar();
    while(input == ' ' || input == ']') input = getchar();
    
    if(input == '['){       // read time range
        TimeRange tr = {get_time(line_ended, ' '), get_time(line_ended, ' ')};
        if(tr.from > tr.to){
            printf("Empty time range!\n");
            exit(0);
        }
        return tr;
    } else{
        int time = get_time(line_ended, input);
        TimeRange tr = {time, time};
        return tr;
    }
}

void print_time_range(TimeRange time){
    if(time.from == time.to){
        print_time(time.from);
        return;
    }

    putchar('[');
    print_time(time.from);
    putchar(',');
    print_time(time.to);
    putchar(']');
}

TimeRange make_operation_time_range(TimeRange time1, TimeRange time2, char op){
    TimeRange res;

    switch (op){
        case '+':
            res.from = time1.from + min(time2.from, time2.to);
            res.to = time1.to + max(time2.from, time2.to);
            break;
        case '-':
            if(time1.from >= time2.from && time1.to <= time2.to){        // 1 is in 2
                printf("Wynikiem jest pusty przedział!\n");
                exit(0);
            } if(time1.from >= time2.from){
                res.from = time2.to + 1;
                res.to = time1.to;
            } else if(time1.to <= time2.to){
                res.from = time1.from;
                res.to = time2.to - 1;
            } else{
                printf("Wynikiem są 2 przedziały czasu!\n");
                exit(0);
            }
            break;
        case '*':
            res.from = time1.from * min(time2.from, time2.to);
            res.to = time1.to * max(time2.from, time2.to);
            break;
        case '/':
            if(time2.from == 0 || time2.to == 0){
                printf("Dzielenie przez 0?!\n");
                exit(0);
            }
            res.from = time1.from / max(time2.from, time2.to);
            res.to = time1.to / min(time2.from, time2.to);
            break;
    }
    return res;
}


int main(){
    TimeRange time1 = {1, 1}, time2 = {1, 1};
    bool line_ended = false;
    char op;

    time1 = get_time_range(&line_ended);
    while(true){
        op = get_op();

        if(op != '\n'){
            time2 = get_time_range(&line_ended);
            time1 = make_operation_time_range(time1, time2, op);
        } else line_ended = true;

        if(line_ended){
            line_ended = false;
            print_time_range(time1);
            putchar('\n');
            time1 = get_time_range(&line_ended);
            if(time1.from == 0 && time1.to == 0) return 0;
        }
    }
}