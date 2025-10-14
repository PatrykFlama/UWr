#include <avr/io.h>
#include <inttypes.h>
#include <stdio.h>
#include <util/delay.h>

#define BAUD 9600                               // baudrate
#define UBRR_VALUE ((F_CPU) / 16 / (BAUD) - 1)  // zgodnie ze wzorem

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

static void print_sep(const char *title) {
    printf("\r\n--- %s ---\r\n", title);
}

#define PRINT_INT_OPS(type, scan_fmt, print_fmt, a, b)                  \
    print_sep("Write a and b for " #type);                              \
    if (scanf(scan_fmt " " scan_fmt, &a, &b) == 2) {                    \
        type sum = a + b;                                               \
        type prod = a * b;                                              \
        printf("a=" print_fmt ", b=" print_fmt "\r\n", a, b);           \
        printf("sum=" print_fmt ", prod=" print_fmt "\r\n", sum, prod); \
        if (b != 0)                                                     \
            printf("div=" print_fmt "\r\n", a / b);                     \
        else                                                            \
            printf("div=undefined (division by zero)\r\n");             \
    }

int main() {
    // zainicjalizuj UART
    uart_init();
    // skonfiguruj strumienie wejścia/wyjścia
    fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
    stdin = stdout = stderr = &uart_file;

    int8_t a8, b8;
    int16_t a16, b16;
    int32_t a32, b32;
    int64_t a64 = 0, b64 = 0;
    float af, bf;

    // // int8_t
    // PRINT_INT_OPS(int8_t, "%" SCNd8, "%" PRId8, a8, b8);

    // // int16_t
    // PRINT_INT_OPS(int16_t, "%" SCNd16, "%" PRId16, a16, b16);

    // // int32_t
    // PRINT_INT_OPS(int32_t, "%" SCNd32, "%" PRId32, a32, b32);

    // start of int8_t function
    print_sep("Write a and b for int8_t");
    if (scanf("%" SCNd8 " %" SCNd8, &a8, &b8) == 2) {                // int8_t: read values a and b
        int8_t sum8 = a8 + b8;                                       // int8_t: calculate sum
        int8_t prod8 = a8 * b8;                                      // int8_t: calculate product
        int8_t div8 = a8 / b8;                                       // int8_t: calculate division
        printf("a=%" PRId8 ", b=%" PRId8 "\r\n", a8, b8);            // int8_t: print a and b
        printf("sum=%" PRId8 ", prod=%" PRId8 "\r\n", sum8, prod8);  // int8_t: print sum and product
        printf("div=%" PRId8 "\r\n", div8);                          // int8_t: print division result
    }

    // start of int16_t function
    print_sep("Write a and b for int16_t");
    if (scanf("%" SCNd16 " %" SCNd16, &a16, &b16) == 2) {                // int16_t: read values a and b
        int16_t sum16 = a16 + b16;                                       // int16_t: calculate sum
        int16_t prod16 = a16 * b16;                                      // int16_t: calculate product
        int16_t div16 = a16 / b16;                                       // int16_t: calculate division
        printf("a=%" PRId16 ", b=%" PRId16 "\r\n", a16, b16);            // int16_t: print a and b
        printf("sum=%" PRId16 ", prod=%" PRId16 "\r\n", sum16, prod16);  // int16_t: print sum and product
        printf("div=%" PRId16 "\r\n", div16);                            // int16_t: print division result
    }

    // start of int32_t function
    print_sep("Write a and b for int32_t");
    if (scanf("%" SCNd32 " %" SCNd32, &a32, &b32) == 2) {                // int32_t: read values a and b
        int32_t sum32 = a32 + b32;                                       // int32_t: calculate sum
        int32_t prod32 = a32 * b32;                                      // int32_t: calculate product
        int32_t div32 = a32 / b32;                                       // int32_t: calculate division
        printf("a=%" PRId32 ", b=%" PRId32 "\r\n", a32, b32);            // int32_t: print a and b
        printf("sum=%" PRId32 ", prod=%" PRId32 "\r\n", sum32, prod32);  // int32_t: print sum and product
        printf("div=%" PRId32 "\r\n", div32);                            // int32_t: print division result
    }

    // start of int64_t function
    print_sep("int64_t (use 32-bit for input)");
    int32_t tmp1, tmp2;                                                                    // int64_t: read values a and b
    if (scanf("%" SCNd32 " %" SCNd32, &tmp1, &tmp2) == 2) {                                // int64_t: read values a and b
        a64 = (int64_t)tmp1;                                                               // int64_t: cast to int64_t
        b64 = (int64_t)tmp2;                                                               // int64_t: cast to int64_t
        int64_t sum64 = a64 + b64;                                                         // int64_t: calculate sum
        int64_t prod64 = a64 * b64;                                                        // int64_t: calculate product
        int64_t div64 = a64 / b64;                                                         // int64_t: calculate division
        printf("a=%" PRId32 ", b=%" PRId32 "\r\n", (int32_t)a64, (int32_t)b64);            // int64_t: print a and b
        printf("sum=%" PRId32 ", prod=%" PRId32 "\r\n", (int32_t)sum64, (int32_t)prod64);  // int64_t: print sum and product
        printf("div=%" PRId32 "\r\n", (int32_t)(div64));                                   // int64_t: print division result
    }

    // start of float function
    print_sep("float");
    if (scanf("%f %f", &af, &bf) == 2) {             // float: read values a and b
        float sumf = af + bf;                        // float: calculate sum
        float prodf = af * bf;                       // float: calculate product
        float divf = af / bf;                        // float: calculate division
        printf("a=%f, b=%f\r\n", af, bf);            // float: print a and b
        printf("sum=%f, prod=%f\r\n", sumf, prodf);  // float: print sum and product
        printf("div=%f\r\n", divf);                  // float: print division result
    }
}
