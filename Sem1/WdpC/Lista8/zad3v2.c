#include "../template.c"

#define FASTFLOOR(x) (((int)(x) <= (x)) ? ((int)x) : (((int)x) - 1))

//---------------------------------------------------------------------
// Static data
unsigned char perm[512] = {151, 160, 137, 91, 90, 15,
                           131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23,
                           190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177, 33,
                           88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71, 134, 139, 48, 27, 166,
                           77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244,
                           102, 143, 54, 65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169, 200, 196,
                           135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64, 52, 217, 226, 250, 124, 123,
                           5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28, 42,
                           223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9,
                           129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97, 228,
                           251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249, 14, 239, 107,
                           49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254,
                           138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180,
                           151, 160, 137, 91, 90, 15,
                           131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23,
                           190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177, 33,
                           88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71, 134, 139, 48, 27, 166,
                           77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244,
                           102, 143, 54, 65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169, 200, 196,
                           135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64, 52, 217, 226, 250, 124, 123,
                           5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28, 42,
                           223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9,
                           129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97, 228,
                           251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249, 14, 239, 107,
                           49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254,
                           138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180};

//---------------------------------------------------------------------
float grad2(int hash, float x, float y) {
    int h = hash & 7;         // Convert low 3 bits of hash code
    float u = h < 4 ? x : y;  // into 8 simple gradient directions,
    float v = h < 4 ? y : x;  // and compute the dot product with (x,y).
    return ((h & 1) ? -u : u) + ((h & 2) ? -2.0f * v : 2.0f * v);
}

// 2D simplex noise
float snoise2(float x, float y) {
#define F2 0.366025403  // F2 = 0.5*(sqrt(3.0)-1.0)
#define G2 0.211324865  // G2 = (3.0-Math.sqrt(3.0))/6.0

    float n0, n1, n2;  // Noise contributions from the three corners

    // Skew the input space to determine which simplex cell we're in
    float s = (x + y) * F2;  // Hairy factor for 2D
    float xs = x + s;
    float ys = y + s;
    int i = FASTFLOOR(xs);
    int j = FASTFLOOR(ys);

    float t = (float)(i + j) * G2;
    float X0 = i - t;  // Unskew the cell origin back to (x,y) space
    float Y0 = j - t;
    float x0 = x - X0;  // The x,y distances from the cell origin
    float y0 = y - Y0;

    // For the 2D case, the simplex shape is an equilateral triangle.
    // Determine which simplex we are in.
    int i1, j1;  // Offsets for second (middle) corner of simplex in (i,j) coords
    if (x0 > y0) {
        i1 = 1;
        j1 = 0;
    }  // lower triangle, XY order: (0,0)->(1,0)->(1,1)
    else {
        i1 = 0;
        j1 = 1;
    }  // upper triangle, YX order: (0,0)->(0,1)->(1,1)

    // A step of (1,0) in (i,j) means a step of (1-c,-c) in (x,y), and
    // a step of (0,1) in (i,j) means a step of (-c,1-c) in (x,y), where
    // c = (3-sqrt(3))/6

    float x1 = x0 - i1 + G2;  // Offsets for middle corner in (x,y) unskewed coords
    float y1 = y0 - j1 + G2;
    float x2 = x0 - 1.0f + 2.0f * G2;  // Offsets for last corner in (x,y) unskewed coords
    float y2 = y0 - 1.0f + 2.0f * G2;

    // Wrap the integer indices at 256, to avoid indexing perm[] out of bounds
    int ii = i & 0xff;
    int jj = j & 0xff;

    // Calculate the contribution from the three corners
    float t0 = 0.5f - x0 * x0 - y0 * y0;
    if (t0 < 0.0f)
        n0 = 0.0f;
    else {
        t0 *= t0;
        n0 = t0 * t0 * grad2(perm[ii + perm[jj]], x0, y0);
    }

    float t1 = 0.5f - x1 * x1 - y1 * y1;
    if (t1 < 0.0f)
        n1 = 0.0f;
    else {
        t1 *= t1;
        n1 = t1 * t1 * grad2(perm[ii + i1 + perm[jj + j1]], x1, y1);
    }

    float t2 = 0.5f - x2 * x2 - y2 * y2;
    if (t2 < 0.0f)
        n2 = 0.0f;
    else {
        t2 *= t2;
        n2 = t2 * t2 * grad2(perm[ii + 1 + perm[jj + 1]], x2, y2);
    }

    // Add contributions from each corner to get the final noise value.
    // The result is scaled to return values in the interval [-1,1].
    return 40.0f * (n0 + n1 + n2);  // TODO: The scale factor is preliminary!
}


//=============================================================
float res_MIN = FLT_MAX;
float res_MAX = FLT_MIN;

typedef struct Point{
    int x, y;
} Point;

int compareX(const void* a, const void* b){
    Point *p1 = (Point *)a,  *p2 = (Point *)b;
    return (p1->x - p2->x);
}

int compareY(const void* a, const void* b){
    Point *p1 = (Point *)a,   *p2 = (Point *)b;
    return (p1->y - p2->y);
}

float dist(Point p1, Point p2){
    if(p1.x == p2.x && p1.y == p2.y) return FLT_MAX;
    return sqrt((p1.x - p2.x)*(p1.x - p2.x) + (p1.y - p2.y)*(p1.y - p2.y));
}

//=============================================================
#define L (int)(7e3 + 5)
Point map[L]; int map_size = 0;    // all local maximas
int next[L];        // distance (in points) from point
int tab[L][L];      // calc tab
bool del[L];        // deleted points

/*
FILE* output_file;

void initialize_file(int h, int w){
    output_file = fopen("mapa.ppm", "w");
    fprintf(output_file, "P3\n%d %d\n255\n", h, w);
}

void print(bool to_file, char what, float f){
    if(to_file){
        if(what == 'f'){
            fprintf(output_file, "%d %d %d ", (int)((f+1)*125), (int)((f+1)*125), (int)((f+1)*125));
        } else if(what == '\n'){
            fprintf(output_file, "\n");
        } else if(what == 'w'){
            fprintf(output_file, "255 255 255 ");
        } else if(what == 'r'){
            fprintf(output_file, "255 0 0 ");
        } else if(what == 'y'){
            fprintf(output_file, "175 200 0 ");  
        } else{
            fprintf(output_file, "0 0 0 ");
        }
    } else{
        printf("%c", what);
    }
}
*/

int compare_dist_arg;
int compareDist(const void* a, const void* b){
    int ai = *(int*) a, bi = *(int*) b;
    Point main_dist = map[compare_dist_arg], p1 = map[ai], p2 = map[bi];
    float ad = dist(main_dist, p1), bd = dist(main_dist, p2);

    if(ad < bd) return -1;
    if(ad > bd) return 1;
    if(ai < bi) return -1;
    if(ai > bi) return 1;
    return 0;
}

void preprocess(int xres, int yres, int points){
    FOR(i, map_size)
        FOR(j, map_size)
            tab[i][j] = j;

    FOR(i, map_size){
        compare_dist_arg = i;
        qsort(tab[i], map_size, sizeof(int), compareDist);
    }
}

void run_once(){
    float min_dist = FLT_MAX;
    int ptr;

    FOR(map_size){
        while(next[i] < map_size && del[tab[i][next[i]]]) next[i]++;        // get rid of deleten points

        float new_dist = dist(map[tab[i][next[i]]], map[i]);                // find smallest distance
        if(min_dist > new_dist){
            min_dist = new_dist;
            ptr = i;
        }
    }

    res_MIN = min(min_dist, res_MIN);

    Point zero = {0,0};
    if(dist(zero, map[ptr]) < dist(zero, map[tab[ptr][next[ptr]]])){        // first element is first/smaller
        del[ptr] = true;
    } else{                                                                 // second element is first/smaller
        del[tab[ptr][next[ptr]]] = true;
    }
}

void delete_towers(int d){
    FOR(d) run_once();
}

//=============================================================

int find_local_max(int xres, int yres, float step) {
    bool vis = (bool)malloc(xres * yres);
    float move[8][2] = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}, {1, 1}, {-1, 1}, {1, -1}, {-1, -1}};

    for(float sx = 1; sx < xres - 1; sx++) {
        for(float sy = 1; sy < yres - 1; sy++) {
            bool local_max = true;
            FOR(8){
            if(snoise2((sx+move[i][0])*step, (sy+move[i][1])*step) >= snoise2(sx*step, sy*step)){
                local_max = false;
            }
            }
            if(local_max){
                map[map_size].x = sx, map[map_size].y = sy;
                map_size++;
            }
        }
    }
}


int main() {
    int xres, yres, k, d;
    scanf("%d%d%d%d", &xres, &yres, &k, &d);
    float step = 1.0 / k;

    find_local_max(xres, yres, step);
    int map_size_pre = map_size;

    delete_towers(d);

    FOR(i, map_size){
        FOR(j, i+1, map_size){
            if(!(del[i] || del[j])) res_MAX = max(res_MAX, dist(map[i], map[j]));
        }
    }

    printf("%d %.1f %d %.1f\n", map_size_pre, res_MIN, map_size_pre - d, res_MAX);
}
