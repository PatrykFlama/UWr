#include <avr/io.h>
#include <stdint.h>
#include <util/delay.h>


static inline void timers_init() {
    // Timer1: Fast PWM, TOP = ICR1 = 255, inverting on OC1A/OC1B, prescaler 64
    // mode: Fast PWM, TOP = ICR1 (WGM13:0 = 14 -> 1110b)
    ICR1 = 255;
    TCCR1A = _BV(COM1A1) | _BV(COM1A0) | _BV(COM1B1) | _BV(COM1B0) | _BV(WGM11); // inverting A,B
    TCCR1B = _BV(WGM13) | _BV(WGM12) | _BV(CS11) | _BV(CS10);                    // prescaler = 64

    // Timer2: Fast PWM 8-bit (TOP = 0xFF), inverting on OC2A, prescaler 64
    TCCR2A = _BV(COM2A1) | _BV(COM2A0) | _BV(WGM21) | _BV(WGM20); // inverting OC2A, fast PWM 8-bit
    TCCR2B = _BV(CS22);                                       // prescaler = 64 (Timer2: CS22=1 -> /64)

    // set pins as outputs (hardware PWM will drive them)
    DDRB |= _BV(PB1) | _BV(PB2) | _BV(PB3);

    // disable outputs initially
    OCR1A = 0;
    OCR1B = 0;
    OCR2A = 0;
}

// 16-bit LFSR random generator
static uint16_t lfsr = 0xBEEF;
static inline uint8_t rand8() {
    // taps: 16 14 13 11; feedback polynomial: x^16 + x^14 + x^13 + x^11 + 1
    const uint16_t bit = ((lfsr >> 0) ^ (lfsr >> 2) ^ (lfsr >> 3) ^ (lfsr >> 5)) & 1u;
    lfsr = (lfsr >> 1) | (bit << 15);
    return (uint8_t)(lfsr & 0xFFu);
}

// return only saturated color
static inline void color_wheel(uint8_t pos, uint8_t* r, uint8_t* g, uint8_t* b) {
    if (pos < 255/3) {
        *r = 255 - pos * 3;
        *g = pos * 3;
        *b = 0;
    } else if (pos < 2 * (255/3)) {
        pos -= 255/3;
        *r = 0;
        *g = 255 - pos * 3;
        *b = pos * 3;
    } else {
        pos -= 2 * (255/3);
        *r = pos * 3;
        *g = 0;
        *b = 255 - pos * 3;
    }
}

int main() {
    timers_init();

    // seed LFSR
    lfsr ^= (uint16_t)TCNT1;

    uint8_t hue = rand8();  // current hue
    uint8_t phase = 0;     // breathing phase
    const uint8_t phase_step = 1; // phase increment per cycle

    while (1) {
        if (phase == 0) {
            hue += rand8() & 0x1F + 0x10;  // ensure significant change in hue
        }

        const uint8_t tri = (phase < 128) ? (uint8_t)(phase << 1) : (uint8_t)((255 - phase) << 1);

        // brightness = (tri * tri) / 255
        const uint16_t bsq = (uint16_t)tri * (uint16_t)tri;
        const uint8_t brightness = (uint8_t)(bsq / 255u) / 2;

        // pick colors
        uint8_t cr, cg, cb;
        color_wheel(hue, &cr, &cg, &cb);

        // scale colors by brightness
        uint16_t tmp;
        const uint8_t sr = (uint8_t)(((uint16_t)cr * (uint16_t)brightness) / 255u);
        const uint8_t sg = (uint8_t)(((uint16_t)cg * (uint16_t)brightness) / 255u);
        const uint8_t sb = (uint8_t)(((uint16_t)cb * (uint16_t)brightness) / 255u);

        // set PWM outputs
        OCR1A = sr;
        OCR1B = sg;
        OCR2A = sb;

        // advance phase
        phase = (uint8_t)(phase + phase_step);

        _delay_ms(8);
    }
}
