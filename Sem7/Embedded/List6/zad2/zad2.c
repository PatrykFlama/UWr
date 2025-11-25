#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/sleep.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <util/delay.h>

#define BAUD 9600
#define UBRR_VALUE ((F_CPU) / 16 / (BAUD) - 1)

#define BUF_BITS 6
#define BUF_SIZE (1u << BUF_BITS)
#define BUF_MASK (BUF_SIZE - 1)  // used instead of modulo

static volatile uint8_t rx_buf[BUF_SIZE];
static volatile uint8_t rx_r = 0;
static volatile uint8_t rx_l = 0;

static volatile uint8_t tx_buf[BUF_SIZE];
static volatile uint8_t tx_r = 0;
static volatile uint8_t tx_l = 0;

// inicjalizacja UART
void uart_init() {
    // ustaw baudrate
    UBRR0 = UBRR_VALUE;
    // wyczyść rejestr UCSR0A
    UCSR0A = 0;
    // włącz odbiornik i nadajnik i przerwanie RX Complete
    UCSR0B = _BV(RXEN0) | _BV(TXEN0) | _BV(RXCIE0);
    // ustaw format 8n1
    UCSR0C = _BV(UCSZ00) | _BV(UCSZ01);
}

// transmit one byte
void uart_transmit(char c, FILE *stream) {
    uint8_t next;

    next = (uint8_t)((tx_r + 1) & BUF_MASK);
    if (next != tx_l) {
        tx_buf[tx_r] = c;
        tx_r = next;
        // enable UDRE interrupt so ISR will start sending
        UCSR0B |= _BV(UDRIE0);
    }
}

// receive one byte
uint8_t uart_receive(FILE *stream) {
    uint8_t c;
    while (1) {
        if (rx_r != rx_l) {
            c = rx_buf[rx_l];
            rx_l = (uint8_t)((rx_l + 1) & BUF_MASK);
            return c;
        }
        // buffer empty -> busy-wait
    }
}

// USART, Rx Complete
ISR(USART_RX_vect) {
    uint8_t c = UDR0;
    UDR0 = c;  // echo back
    uint8_t next = (uint8_t)((rx_r + 1) & BUF_MASK);
    if (next != rx_l) {
        rx_buf[rx_r] = c;
        rx_r = next;
    } else {
        // buffer full -> drop byte
    }
}

// USART Data Register Empty
ISR(USART_UDRE_vect) {
    if (tx_l != tx_r) {
        UDR0 = tx_buf[tx_l];
        tx_l = (uint8_t)((tx_l + 1) & BUF_MASK);
        _delay_ms(1);
    } else {
        // nothing more to send -> disable UDRE interrupt
        UCSR0B &= ~_BV(UDRIE0);
    }
}


FILE uart_file;

// stdio wrapper: putchar-style (returns int)
int uart_putchar(char data, FILE *stream) {
    uint8_t next = (uint8_t)((tx_r + 1) & BUF_MASK);
    if (next != tx_l) {
        tx_buf[tx_r] = (uint8_t)data;
        tx_r = next;
        // enable UDRE interrupt so ISR will start sending
        UCSR0B |= _BV(UDRIE0);
    }
    return 0;
}

// stdio wrapper: getchar-style (returns int or EOF)
int uart_getchar(FILE *stream) {
    uint8_t c;
    while (1) {
        if (rx_r != rx_l) {
            c = rx_buf[rx_l];
            rx_l = (uint8_t)((rx_l + 1) & BUF_MASK);
            return (int)c;
        }
        // buffer empty -> busy-wait
    }
    return EOF;
}

int main() {
    uart_init();
    sei();
    fdev_setup_stream(&uart_file, uart_putchar, uart_getchar, _FDEV_SETUP_RW);
    stdin = stdout = stderr = &uart_file;

    while (1) {
        printf("Type string: \r\n");
        sleep_mode();
        char buf[100];

        // read a whitespace-delimited token
        if (scanf("%99s", buf) == 1) {
            printf("You typed: %s\r\n", buf);
        }
    }
}
