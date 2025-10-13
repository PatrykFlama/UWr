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

const int DELAY_BETWEEN_WORDS = 500;
const int DELAY_BETWEEN_LETTERS = 50;
const int DELAY_LONG = 300;
const int DELAY_SHORT = 100;

const char *morse_code[256] = {
    ['A'] = ".-", ['B'] = "-...", ['C'] = "-.-.", ['D'] = "-..", ['E'] = ".", ['F'] = "..-.", ['G'] = "--.", ['H'] = "....", ['I'] = "..", ['J'] = ".---", ['K'] = "-.-", ['L'] = ".-..", ['M'] = "--", ['N'] = "-.", ['O'] = "---", ['P'] = ".--.", ['Q'] = "--.-", ['R'] = ".-.", ['S'] = "...", ['T'] = "-", ['U'] = "..-", ['V'] = "...-", ['W'] = ".--", ['X'] = "-..-", ['Y'] = "-.--", ['Z'] = "--..", ['0'] = "-----", ['1'] = ".----", ['2'] = "..---", ['3'] = "...--", ['4'] = "....-", ['5'] = ".....", ['6'] = "-....", ['7'] = "--...", ['8'] = "---..", ['9'] = "----.", [' '] = "/"};

#define blink_led(ms)     \
    LED_PORT |= _BV(LED); \
    _delay_ms(ms);        \
    LED_PORT &= ~_BV(LED);

void blink_morse(const char *text) {
    for (const char *p = text; *p != '\0'; p++) {
        unsigned char uc = (unsigned char)*p;
        char c = (char)toupper(uc);

        const char *code = morse_code[(unsigned char)c];
        if (code == NULL) continue;

        for (const char *q = code; *q != '\0'; q++) {
            char symbol = *q;
            if (symbol == '.') {
                blink_led(DELAY_SHORT);
            } else if (symbol == '-') {
                blink_led(DELAY_LONG);
            }
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
