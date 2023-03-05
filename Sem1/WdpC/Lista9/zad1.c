#include "../template.c"

#define R 6371      // earth radious in km
const double PI = 3.14159265359;

FILE *input;


typedef struct Point{
    double x, y, z;
} Point;

double dist(Point p1, Point p2){
    return sqrt((p2.x-p1.x)*(p2.x-p1.x) + (p2.y-p1.y)*(p2.y-p1.y) + (p2.z-p1.z)*(p2.z-p1.z));
}

int main(int argc, char** argv){
    if(argc < 2) exit(0);
    input = fopen(argv[1], "r");

    double max_dist = 0;
    int row_num = 0, tracking_points = 0;

    char str[200];
    Point now, last;
    while(fgets(str, 200, input) && !feof(input)){
        row_num++;
        if(strstr(str, "<trkpt") == NULL) continue;
        tracking_points++;
        
        char* lat = strstr(str, "lat=");
        char* lon = strstr(str, "lon=");

        lat += 5, lon += 5;
        char *lat_end = lat, *lon_end = lon;
        while(*lat_end != '"') lat_end++;
        while(*lon_end != '"') lon_end++;
        *lat_end = '\0';
        *lon_end = '\0';
        lat_end--, lon_end--;
        
        double dlat = atof(lat), dlon = atof(lon);
        dlat = dlat*PI/180.0, dlon = dlon*PI/180.0;
        
        now.x = R * cos(dlat) * cos(dlon);
        now.y = R * cos(dlat) * sin(dlon);
        now.z = R * sin(dlat);
        
        if(tracking_points > 1) {max_dist += dist(last, now);
        printf("%lf\n", dist(last, now)*1000);
        }
        last.x = now.x, last.y = now.y, last.z = now.z;
    }

    printf("Size in bytes: %d\nNumber of rows: %d\n", ftell(input), row_num+1);
    printf("Tracking points: %d\n", tracking_points);
    printf("Track length: %lf\n", max_dist*1000);
    fclose(input);
}
