#include <avr/io.h>
#include <inttypes.h>
#include <stdio.h>
#include <util/delay.h>

static const uint8_t digits[10] = {
    // DP G F E D C B A
    0b11000000,  // 0
    0b11111001,  // 1
    0b10100100,  // 2
    0b10110000,  // 3
    0b10011001,  // 4
    0b10010010,  // 5
    0b10000010,  // 6
    0b11111000,  // 7
    0b10000000,  // 8
    0b10010000   // 9
};

int main(void) {
    DDRD = 0xFF;

    UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);

    uint8_t i = 0;
    while (1) {
        PORTD = digits[i];
        _delay_ms(1000);
        i++;
        if (i >= 10) i = 0;
    }
}
