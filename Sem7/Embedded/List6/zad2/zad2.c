#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>
#include <stdint.h>

#define BAUD 9600
#define UBRR_VALUE ((F_CPU) / 16 / (BAUD) - 1)

#define BUF_BITS 6
#define BUF_SIZE (1u << BUF_BITS)
#define BUF_MASK (BUF_SIZE - 1)     // used instead of modulo

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
void uart_transmit(uint8_t c) {
    uint8_t next;
    while (1) {
        next = (uint8_t)((tx_r + 1) & BUF_MASK);
        if (next != tx_l) {
            tx_buf[tx_r] = c;
            tx_r = next;
            // enable UDRE interrupt so ISR will start sending
            UCSR0B |= _BV(UDRIE0);
            return;
        }
        // buffer full -> busy-wait
    }
}

// receive one byte
uint8_t uart_receive() {
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
    } else {
        // nothing more to send -> disable UDRE interrupt
        UCSR0B &= ~_BV(UDRIE0);
    }
}

int main() {
    uart_init();
    set_sleep_mode(SLEEP_MODE_IDLE);
    sei();

    while (1) {
        sleep_mode();
        uint8_t c = uart_receive();
        uart_transmit(c);

        if (c == '\r') {
            uart_transmit('\n');
        }
    }
}
