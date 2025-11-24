#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/sleep.h>
#include <stdint.h>

#define BAUD 9600
#define UBRR_VALUE ((F_CPU) / 16 / (BAUD) - 1)

volatile uint8_t tx_byte = 0;
volatile uint8_t tx_pending = 0;  // flag

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

// USART, Rx Complete
ISR(USART_RX_vect) {
    const uint8_t c = UDR0;
    tx_byte = c;
    tx_pending = 1;
    UCSR0B |= _BV(UDRIE0);  // enable interrupt USART Data Register Empty
}

// USART Data Register Empty
ISR(USART_UDRE_vect) {
    if (tx_pending) {
        UDR0 = tx_byte;
        tx_pending = 0;
    }

    // disable UDRE interrupts as no more data to send
    UCSR0B &= ~_BV(UDRIE0);
}

int main() {
    uart_init();

    set_sleep_mode(SLEEP_MODE_IDLE);

    sei();

    while (1) {
        sleep_mode();
    }
}
