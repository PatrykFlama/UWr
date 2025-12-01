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

    #ifdef UART_ECHO
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
    #endif
    
    return UDR0;
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

static void uart_readline(char *buf, size_t size) {
    size_t idx = 0;
    int c;
    while (1) {
        c = getchar();
        if (c == EOF) continue;
        if (c == '\r' || c == '\n') {
            // echo CRLF
            putchar('\r'); putchar('\n');
            break;
        }
        if (c == 0x7f || c == 0x08) { // backspace
            if (idx > 0) {
                idx--;
                // erase on terminal
                putchar('\b'); putchar(' '); putchar('\b');
            }
            continue;
        }
        // printable
        if (idx < size - 1) {
            buf[idx++] = (char)c;
            putchar((char)c);
        } else {
            // buffer full, still echo
            putchar((char)c);
        }
    }
    buf[idx] = '\0';
}
