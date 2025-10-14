#include <avr/io.h>
#include <ctype.h>
#include <inttypes.h>
#include <stdio.h>
#include <string.h>
#include <util/delay.h>

#define BAUD 9600                               // baudrate
#define UBRR_VALUE ((F_CPU) / 16 / (BAUD) - 1)  // zgodnie ze wzorem

#define LED PC0
#define LED_DDR DDRC
#define LED_PORT PORTC

const int DELAY_BETWEEN_WORDS = 700;
const int DELAY_BETWEEN_LETTERS = 100;
const int DELAY_LONG = 300;
const int DELAY_SHORT = 100;

//! to jest obrzydliwe - tak nie chcemy pisać
// const char* morse_code[256] = {
//     [ 'A'] = ".-",   [ 'B'] = "-...", [ 'C'] = "-.-.", [ 'D'] = "-..",  [ 'E'] = ".",
//     [ 'F'] = "..-.", [ 'G'] = "--.",  [ 'H'] = "....", [ 'I'] = "..",   [ 'J'] = ".---",
//     [ 'K'] = "-.-",  [ 'L'] = ".-..", [ 'M'] = "--",   [ 'N'] = "-.",   [ 'O'] = "---",
//     [ 'P'] = ".--.", [ 'Q'] = "--.-", [ 'R'] = ".-.",  [ 'S'] = "...",  [ 'T'] = "-",
//     [ 'U'] = "..-",  [ 'V'] = "...-", [ 'W'] = ".--",  [ 'X'] = "-..-", [ 'Y'] = "-.--",
//     [ 'Z'] = "--..",
//     [ '0'] = "-----",[ '1'] = ".----",[ '2'] = "..---",[ '3'] = "...--",[ '4'] = "....-",
//     [ '5'] = ".....",[ '6'] = "-....",[ '7'] = "--...",[ '8'] = "---..",[ '9'] = "----.",
//     [ ' '] = ""
// };

const int L = 'z'-'a'+1+10;
// 3 bits for size and rest for code
const int morse_code_better[] = {
    0b01001000,
    0b10010000,
    0b10010100,
    0b01110000,
    0b00100000,
    0b10000100,
    0b01111000,
    0b10000000,
    0b01000000,
    0b10001110,
    0b01110100,
    0b10001000,
    0b01011000,
    0b01010000,
    0b01111100,
    0b10001100,
    0b10011010,
    0b01101000,
    0b01100000,
    0b00110000,
    0b01100100,
    0b10000010,
    0b01101100,
    0b10010010,
    0b10010110,
    0b10011000,
    0b10111111,
    0b10101111,
    0b10100111,
    0b10100011,
    0b10100001,
    0b10100000,
    0b10110000,
    0b10111000,
    0b10111100,
    0b10111110
};

#define blink_led(ms)     \
    LED_PORT |= _BV(LED); \
    _delay_ms(ms);        \
    LED_PORT &= ~_BV(LED);

void blink_morse(const char *text) {
    for (const char *p = text; *p != '\0'; p++) {
        unsigned char uc = (unsigned char)*p;
        char c = (char)toupper(uc);

        // const char *code = morse_code[(unsigned char)c];
        int code;
        if (c >= 'A' && c <= 'Z') code = morse_code_better[c-'A'];
        else if (c >= '0' && c <= '9') code = morse_code_better[c-'0'];
        else continue;

        int len = (code & 0b11100000) >> 5;
        for (int i = 0; i < len; i++) {
            int symbol = (code & (0b00010000 >> i));
            if (!symbol) {
                blink_led(DELAY_SHORT);
            } else {
                blink_led(DELAY_LONG);
            }
            // if (symbol == '.') {
            //     blink_led(DELAY_SHORT);
            // } else if (symbol == '-') {
            //     blink_led(DELAY_LONG);
            // }
            _delay_ms(DELAY_BETWEEN_LETTERS);
        }

        _delay_ms(DELAY_BETWEEN_WORDS);
    }
}

// inicjalizacja UART
void uart_init() {
    // ustaw baudrate
    UBRR0 = UBRR_VALUE;
    // wyczyść rejestr UCSR0A
    UCSR0A = 0;
    // włącz odbiornik i nadajnik
    UCSR0B = _BV(RXEN0) | _BV(TXEN0);
    // ustaw format 8n1
    UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);
}

// transmisja jednego znaku
int uart_transmit(char data, FILE *stream) {
    // czekaj aż transmiter gotowy
    while (!(UCSR0A & _BV(UDRE0)));
    UDR0 = data;
    return 0;
}

// odczyt jednego znaku
int uart_receive(FILE *stream) {
    // czekaj aż znak dostępny
    while (!(UCSR0A & _BV(RXC0)));
    return UDR0;
}

FILE uart_file;

void setup() {
    LED_DDR |= _BV(LED);
    LED_PORT &= ~_BV(LED);

    // zainicjalizuj UART
    uart_init();
    // skonfiguruj strumienie wejścia/wyjścia
    fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
    stdin = stdout = stderr = &uart_file;
}

int main() {
    setup();

    // program testowy
    while (1) {
        printf("Podaj tekst:\r\n");
        char buf[128];
        if (scanf("%127s", buf) != 1) {
            continue;
        }

        printf("Odczytano: %s\r\n", buf);
        printf("Nadawanie... ");
        blink_morse(buf);
        printf("OK\r\n");
    }
}
