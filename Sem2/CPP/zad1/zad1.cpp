#include <bits/stdc++.h>
using namespace std;

const vector< pair<int, string> > rzymskie = {
    {1000, "M"}, {900, "CM"}, {500, "D"}, {400, "CD"}, 
    {100, "C"},  {90, "XC"},  {50, "L"},  {40, "XL"},
    {10, "X"}, {9, "IX"}, {5, "V"}, {4, "IV"}, {1, "I"}
};


string bin2rzym (int x){
    string res = "";
    for(int i = 0; i < rzymskie.size(); i++){
        while(x >= rzymskie[i].first){
            res += rzymskie[i].second;
            x -= rzymskie[i].first;
        }
    }

    return res;
}


int main(int argc, char** argv){
    for(int i = 1; i < argc; i++){
        int n;
        try{
            n = stoi(argv[i]);
            if(1 > n || n > 3999){
                clog << "Podaj liczbe z przedzialu [1, 3999]!\n";
                continue;
            }
        }
        catch(...){
            clog << "Nie podano poprawdnej liczby na wejsciu\n";
            continue;
        }

        cout << bin2rzym(n) << endl;
    }
}