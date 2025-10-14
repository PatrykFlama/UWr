#include <bits/stdc++.h>

#include <unordered_map>

const int L = 26 + 10;
std::pair<char, const char*> morse_code[] = {
    {'A', ".-"}, {'B', "-..."}, {'C', "-.-."}, {'D', "-.."}, {'E', "."}, {'F', "..-."}, {'G', "--."}, {'H', "...."}, {'I', ".."}, {'J', ".---"}, {'K', "-.-"}, {'L', ".-.."}, {'M', "--"}, {'N', "-."}, {'O', "---"}, {'P', ".--."}, {'Q', "--.-"}, {'R', ".-."}, {'S', "..."}, {'T', "-"}, {'U', "..-"}, {'V', "...-"}, {'W', ".--"}, {'X', "-..-"}, {'Y', "-.--"}, {'Z', "--.."},
    {'0', "-----"}, {'1', ".----"}, {'2', "..---"}, {'3', "...--"}, {'4', "....-"}, {'5', "....."}, {'6', "-...."}, {'7', "--..."}, {'8', "---.."}, {'9', "----."}
};

char to_char(int x) {
    if (x == 0) return '0';
    return '1';
}

int main() {
    for (int i = 0; i < L; i++) {
        const int len = strlen(morse_code[i].second);
        printf("0b%c%c%c", to_char(len & 4), to_char(len & 2), to_char(len & 1));
        for (int j = 0; j < len; j++) {
            if (morse_code[i].second[j] == '.')
                printf("0");
            else
                printf("1");
        }
        for (int j = len; j < 8-3; j++) printf("0");
        printf("\r\n");
    }
}
