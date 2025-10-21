#include <avr/io.h>
#include <util/delay.h>
#include "../../customlib/general.c"
#include "../../customlib/millis.c"

#define LEDS 0xFF
#define LEDS_DDR DDRD
#define LEDS_PORT PORTD

#define DISPLAYS 0b00000011
#define DISPLAY1 PC0
#define DISPLAY2 PC1
#define DISPLAYS_DDR DDRC
#define DISPLAYS_PORT PORTC

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

int digit1 = 0, digit2 = 0;
int8_t displays = 1;

void swap_displays() {
    const int8_t active_display = DISPLAYS_PORT & DISPLAYS;
    const uint8_t is_active_display1 = displays & _BV(DISPLAY1);
    displays ^= DISPLAYS;

    DISPLAYS_PORT = DISPLAYS_PORT & DISPLAYS;  // disable both displays
    LEDS_PORT = digits[is_active_display1 ? digit2 : digit1];     // draw new number
    DISPLAYS_PORT = displays; // enable other display
}

int main() {
    RXTX_DISABLE();
    LEDS_DDR |= LEDS;
    DISPLAYS_DDR |= DISPLAYS;

    timer_init_ms();

    unsigned int start_time = millis();
    int number = 0;
    while (1) {
        if (millis() - start_time >= 1000) {
            // start_time += 1000;
            start_time = millis();

            number = (number + 1) % 60;

            digit1 = number % 10;
            digit2 = (number / 10) % 10;
        }

        _delay_ms(5);
        swap_displays();

        // --- debug ---
        // DISPLAYS_PORT = _BV(DISPLAY1);
        // LEDS_PORT = digits[8];
        // _delay_ms(500);
        // DISPLAYS_PORT = _BV(DISPLAY2);
        // LEDS_PORT = digits[8];
        // _delay_ms(500);
    }
}
