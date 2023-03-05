#include <bits/stdc++.h>
using namespace std;
const int L = 100;
int prefix[L];


int main(){
    string str; cin >> str;
    int n; cin >> n;

    int maxx = 0, max_ptr = 0;
    int number_of_bits = 0;

    // TODO if n > str.size()

    // create prefix table which shows how many bits are on in previos n cells
    if(str[0] == '1') prefix[0] = 1;
    for(int c = 0; c < n-1; c++) prefix[c+1] = prefix[c] + (str[c+1] == '1' ? 1 : 0);
    number_of_bits = prefix[n-1];

    for(int c = n-1; c < str.size(); c++){
        prefix[c] = prefix[c-1];
        if(str[c-n] == '1') prefix[c]--;
        if(str[c] == '1') {
            ++number_of_bits;
            prefix[c]++;
        }

        if(maxx < prefix[c]){
            maxx = prefix[c];
            max_ptr = c;
        }
    }

    int bits_to_switch = n - maxx;   // number of off bits in 'maximal on bits' range
    bits_to_switch += number_of_bits - maxx;         // bits out of 'max on bits' range

    if(true){
        for(int i = 0; i < str.size(); i++) cout << prefix[i];
        cout << '\n';
        cout << maxx << ' ' << max_ptr << ' ' << number_of_bits << '\n';
    }

    cout << bits_to_switch << '\n';
}
