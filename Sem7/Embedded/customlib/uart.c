#pragma once

#include <avr/io.h>
#include <stdio.h>

#define BAUD 9600
#define UBRR_VALUE ((F_CPU) / 16 / (BAUD) - 1)

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

    #ifdef UART_NO_ECHO
    return UDR0;
    #endif

    char c = UDR0;
    // echo received character back to sender
    // map LF or CR to CR+LF for terminal compatibility
    if (c == '\r' || c == '\n') {
        uart_transmit('\r', stream);
        uart_transmit('\n', stream);
    } else {
        uart_transmit(c, stream);
    }
    return (int)c;
}

FILE uart_file;

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

    // skonfiguruj strumienie wejścia/wyjścia
    fdev_setup_stream(&uart_file, uart_transmit, uart_receive, _FDEV_SETUP_RW);
    stdin = stdout = stderr = &uart_file;
}
