#include <bits/stdc++.h>

#include <unordered_map>

std::unordered_map<char, const char*> morse_code = {
    { 'A', ".-" },   { 'B', "-..." }, { 'C', "-.-." }, { 'D', "-.." },  { 'E', "." },
    { 'F', "..-." }, { 'G', "--." },  { 'H', "...." }, { 'I', ".." },   { 'J', ".---" },
    { 'K', "-.-" },  { 'L', ".-.." }, { 'M', "--" },   { 'N', "-." },   { 'O', "---" },
    { 'P', ".--." }, { 'Q', "--.-" }, { 'R', ".-." },  { 'S', "..." },  { 'T', "-" },
    { 'U', "..-" },  { 'V', "...-" }, { 'W', ".--" },  { 'X', "-..-" }, { 'Y', "-.--" },
    { 'Z', "--.." },
    { '0', "-----" },{ '1', ".----" },{ '2', "..---" },{ '3', "...--" },{ '4', "....-" },
    { '5', "....." },{ '6', "-...." },{ '7', "--..." },{ '8', "---.." },{ '9', "----." },
    { ' ', "" }
};

int main() {

        for (int i = 0; i < 256; i++) {
            if (morse_code[i] == NULL) continue;
            const int len = strlen(morse_code[i]);
            printf("0b%c%c%c", len%4, len%2, len%1);
            for (int j = 0; j < len; j++) {
                if (morse_code[i][j] == '.') printf("0");
                else printf("1");
            }
            for (int j = len; j < 8; j++) printf("0");
            printf("\r\n");
        }
}
