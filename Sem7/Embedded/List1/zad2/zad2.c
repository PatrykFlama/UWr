#include <avr/io.h>
#include <inttypes.h>
#include <stdio.h>
#include <util/delay.h>

#define STEP_DELAY_MS 100


int main() {
    // disable RX/TX 
    UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);

    // set PD0..PD7 as outputs (whole PORTD)
    DDRD = 0xFF;

    // start pattern: single lit LED on the left
    uint8_t pattern = 0b00000001;
    // direction: 1 = shift left, 0 = shift right
    uint8_t dir_left = 1;

    while (1) {
        PORTD = pattern;

        _delay_ms(STEP_DELAY_MS);

        pattern = dir_left ? (pattern << 1) : (pattern >> 1);

        if (pattern == 0b10000000 || pattern == 0b00000001) {
            dir_left = 1 - dir_left;
        }
    }
}
