#include <bits/stdc++.h>
using namespace std;

enum DIRS {UP, RIGHT, DOWN, LEFT};
const string DIRS_STR[] = {"U", "R", "D", "L"};

class Point {
public:
    int x, y;
    Point(int x, int y) : x(x), y(y) {}
    Point() : x(0), y(0) {}
    Point(const Point& p) : x(p.x), y(p.y) {}
    Point& operator=(const Point& p) {
        x = p.x;
        y = p.y;
        return *this;
    }
    Point operator+(const Point& p) const {
        return Point(x + p.x, y + p.y);
    }
    int distance(const Point& p) const {
        return (x - p.x) * (x - p.x) + (y - p.y) * (y - p.y);
    }
};

int main()
{
    for (int i = 0; i < 10; i++) {
        string line;
        getline(cin, line);
    }
    int robot_count;
    cin >> robot_count; cin.ignore();
    for (int i = 0; i < robot_count; i++) {
        int x;
        int y;
        string direction;
        cin >> x >> y >> direction; cin.ignore();
    }

    // Write an action using cout. DON'T FORGET THE "<< endl"
    // To debug: cerr << "Debug messages..." << endl;

    cout << "0 0 U 1 1 R 2 2 D 3 3 L" << endl;
}