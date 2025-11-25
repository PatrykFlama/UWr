#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/sleep.h>
#include <stdint.h>
#include <util/delay.h>


#define BAUD 9600
#define UBRR_VALUE ((F_CPU) / 16 / (BAUD) - 1)


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

ISR(USART_RX_vect) {
    UDR0 = UDR0;
}

int main() {
    uart_init();

    set_sleep_mode(SLEEP_MODE_IDLE);

    sei();

    while (1) {
        sleep_mode();
    }
}
