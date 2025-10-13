#include <avr/io.h>
#include <stdio.h>
#include <inttypes.h>
#include <util/delay.h>


#define BAUD 9600
#define UBRR_VALUE ((F_CPU)/16/(BAUD)-1)

#define STEP_DELAY_MS 100

int main() {
    UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);

	// set PD0..PD7 as outputs (whole PORTD)
	DDRD = 0xFF;

	// start pattern: single lit LED on the left
	uint8_t pattern = 0x01;
	// direction: 1 = shift left, 0 = shift right
	uint8_t dir_left = 1;

	while (1) {
		PORTD = pattern;

		_delay_ms(STEP_DELAY_MS);

		if (dir_left) {
			if (pattern == 0x80) {
				dir_left = 0;
				pattern >>= 1;
			} else {
				pattern <<= 1;
			}
		} else {
			if (pattern == 0x01) {
				dir_left = 1;
				pattern <<= 1;
			} else {
				pattern >>= 1;
			}
		}
	}
}
