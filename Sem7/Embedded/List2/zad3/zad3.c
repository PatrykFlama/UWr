#include <avr/io.h>
#include <util/delay.h>
#include "../../customlib/general.c"

#define BTNS 0b00011100
#define BTN_PIN PINB
#define BTN_PORT PORTB

#define LEDS 0xFF
#define LEDS_DDR DDRD
#define LEDS_PORT PORTD

uint8_t to_gray(uint8_t num) {
    return num ^ (num >> 1);
}

typedef void (*btn_function_t)();

int gray_number = 0;

void btn_reset(void) {
    gray_number = 0;
}
void btn_prev(void) {
    gray_number++;
}
void btn_next(void) {
    gray_number--;
}

struct ButtonWithFunction
{
    uint8_t btn_mask;
    btn_function_t function;
};

struct ButtonWithFunction button_to_function[3] = {
    { .btn_mask = _BV(PB2), .function = btn_reset },
    { .btn_mask = _BV(PB3), .function = btn_prev },
    { .btn_mask = _BV(PB4), .function = btn_next },
};


int main() {
    RXTX_DISABLE();

    BTN_PORT |= BTNS;
    LEDS_DDR |= LEDS;

    // use the global gray_number (initialized above)
    gray_number = 0;
    int last_pressed = 0;

    while(1) {
        int raw = BTN_PIN & BTNS;
        int pressed = (~raw) & BTNS;
        int newly_pressed = pressed & ~last_pressed;

        // handle button presses
        for (int i = 0; i < 3; i++) {
            if (pressed & button_to_function[i].btn_mask) {
                button_to_function[i].function();
            }
        }

        // // handle button presses with a small debounce: re-sample after 20 ms
        // if (newly_pressed) {
        //     _delay_ms(20);
        //     int raw2 = BTN_PIN & BTNS;
        //     int pressed2 = (~raw2) & BTNS;
        //     int newly2 = newly_pressed & pressed2; // only keep still-pressed
        //     for (int i = 0; i < 3; i++) {
        //         if (newly2 & button_to_function[i].btn_mask) {
        //             button_to_function[i].function();
        //         }
        //     }
        // }

        // update LEDs with Gray code (only LED bits)
        const uint8_t out = to_gray(gray_number);
        LEDS_PORT = out & LEDS;

        last_pressed = pressed;

        if (pressed) {
            // _delay_ms(10);
            _delay_ms(200);
        }
    }
}
