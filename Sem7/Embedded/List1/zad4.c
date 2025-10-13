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

int main(void) {
    uart_init();
    fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
    stdin = stdout = stderr = &uart_file;

    // Buffer for user input prompts
    int8_t a8, b8;
    int16_t a16, b16;
    int32_t a32, b32;
    int64_t a64 = 0, b64 = 0;
    float af, bf;

    // int8_t
    print_sep("int8_t\n");
    if (scanf("%" SCNd8 " %" SCNd8, &a8, &b8) == 2) {
        int8_t sum = a8 + b8;
        int8_t prod = a8 * b8;
        printf("a=%" PRId8 ", b=%" PRId8 "\r\n", a8, b8);
        printf("sum=%" PRId8 ", prod=%" PRId8 "\r\n", sum, prod);

        if (b8 != 0)
            printf("div=%" PRId8 "\r\n", a8 / b8);
        else
            printf("div=undefined (division by zero)\r\n");
    }

    // int16_t
    print_sep("int16_t\n");
    if (scanf("%" SCNd16 " %" SCNd16, &a16, &b16) == 2) {
        int16_t sum = a16 + b16;
        int16_t prod = a16 * b16;
        printf("a=%" PRId16 ", b=%" PRId16 "\r\n", a16, b16);
        printf("sum=%" PRId16 ", prod=%" PRId16 "\r\n", sum, prod);

        if (b16 != 0)
            printf("div=%" PRId16 "\r\n", a16 / b16);
        else
            printf("div=undefined (division by zero)\r\n");
    }

    // int32_t
    print_sep("int32_t\n");
    if (scanf("%" SCNd32 " %" SCNd32, &a32, &b32) == 2) {
        int32_t sum = a32 + b32;
        int32_t prod = a32 * b32;
        printf("a=%" PRId32 ", b=%" PRId32 "\r\n", a32, b32);
        printf("sum=%" PRId32 ", prod=%" PRId32 "\r\n", sum, prod);
        if (b32 != 0)
            printf("div=%" PRId32 "\r\n", a32 / b32);
        else
            printf("div=undefined (division by zero)\r\n");
    }

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

    // End
    printf("\r\nAll done.\r\n");

    // Keep MCU running
    while (1) _delay_ms(1000);

    return 0;
}
