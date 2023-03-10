#include <bits/stdc++.h>
using namespace std;


class Point{
    public:
    double x, y;
    Point(double _x = 0, double _y = 0) : x(_x), y(_y) {}
    Point(const Point& p){
        x = p.x, y = p.y;
    }

    // pair<int, int> get_coords(){
    //     return {x, y};
    // }

    bool check_lnz(Point p1, Point p2){
        if(p1.x == x && p2.x == x) return false;
        if(p1.y == y && p2.y == y) return false;
        return true;
    }
};

class Segment{
    public:
    Point a, b;
    Segment(Point _a, Point _b) : a(_a), b(_b) {}
    Segment(const Segment& s){
        a = s.a, b = s.b;
    }

    double length(){
        // double diff_x = abs(a.get_coords().first - b.get_coords().first);
        // double diff_y = abs(a.get_coords().second - b.get_coords().second);
        double diff_x = abs(a.x - b.x);
        double diff_y = abs(a.y - b.y);

        return sqrt(diff_x*diff_x + diff_y*diff_y);
    }
};

class Triangle{
    Point a, b, c;

    public:
    Triangle(Point _a, Point _b, Point _c) : a(_a), b(_b), c(_c) {}
    Triangle(const Triangle& t){
        a = t.a, b = t.b, c = t.c;
    }

    bool check_if_correct(){
        Segment s1(a, b), s2(b, c), s3(c, a);
        {
            double s1_l = s1.length(), s2_l = s2.length(), s3_l = s3.length();
            vector<double> lenghts = {s1_l, s2_l, s3_l};
            sort(lenghts.begin(), lenghts.end());

            if(lenghts[2] <= lenghts[0]+lenghts[1]) return false;
        }

        return a.check_lnz(b, c);
    }
};


int main(){

}
