#include <avr/io.h>
#include <inttypes.h>
#include <stdio.h>
#include <util/delay.h>

#define BAUD 9600                               // baudrate
#define UBRR_VALUE ((F_CPU) / 16 / (BAUD) - 1)  // zgodnie ze wzorem

FILE uart_file;

void uart_init() {
    UBRR0 = UBRR_VALUE;
    UCSR0A = 0;
    UCSR0B = _BV(RXEN0) | _BV(TXEN0);
    UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);
}

int uart_transmit(char data, FILE *stream) {
    while (!(UCSR0A & _BV(UDRE0)));
    UDR0 = data;
    return 0;
}

int uart_receive(FILE *stream) {
    while (!(UCSR0A & _BV(RXC0)));
    return UDR0;
}

static void print_sep(const char *title) {
    printf("\r\n--- %s ---\r\n", title);
}

#define PRINT_INT_OPS(type, scan_fmt, print_fmt, a, b) \
    print_sep(#type); \
    if (scanf(scan_fmt " " scan_fmt, &a, &b) == 2) { \
        type sum = a + b; \
        type prod = a * b; \
        printf("a=" print_fmt ", b=" print_fmt "\r\n", a, b); \
        printf("sum=" print_fmt ", prod=" print_fmt "\r\n", sum, prod); \
        if (b != 0) \
            printf("div=" print_fmt "\r\n", a / b); \
        else \
            printf("div=undefined (division by zero)\r\n"); \
    }

int main(void) {
    uart_init();
    fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
    stdin = stdout = stderr = &uart_file;

    int8_t a8, b8;
    int16_t a16, b16;
    int32_t a32, b32;
    int64_t a64 = 0, b64 = 0;
    float af, bf;

    // int8_t
    PRINT_INT_OPS(int8_t, "%" SCNd8, "%" PRId8, a8, b8);

    // int16_t
    PRINT_INT_OPS(int16_t, "%" SCNd16, "%" PRId16, a16, b16);

    // int32_t
    PRINT_INT_OPS(int32_t, "%" SCNd32, "%" PRId32, a32, b32);

    // int64_t
    print_sep("int64_t (use 32-bit for input)");
    int tmp1, tmp2;
    if (scanf("%" SCNd32 " %" SCNd32, &tmp1, &tmp2) == 2) {
        a64 = (int64_t)tmp1;
        b64 = (int64_t)tmp2;
        int64_t sum = a64 + b64;
        int64_t prod = a64 * b64;
        printf("a=%" PRId32 ", b=%" PRId32 "\r\n", (int32_t)a64, (int32_t)b64);
        printf("sum=%" PRId32 ", prod=%" PRId32 "\r\n", (int32_t)sum, (int32_t)prod);

        if (b64 != 0)
            printf("div=%" PRId32 "\r\n", (int32_t)(a64 / b64));
        else
            printf("div=undefined (division by zero)\r\n");
    }

    // float
    print_sep("float");
    if (scanf("%f %f", &af, &bf) == 2) {
        float sumf = af + bf;
        float prodf = af * bf;
        printf("a=%f, b=%f\r\n", af, bf);
        printf("sum=%f, prod=%f\r\n", sumf, prodf);
        if (bf != 0.0f)
            printf("div=%f\r\n", af / bf);
        else
            printf("div=undefined (division by zero)\r\n");
    }

    while (1) _delay_ms(1000);
}
