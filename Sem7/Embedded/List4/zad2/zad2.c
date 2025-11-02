#include <avr/io.h>
#include <util/delay.h>
#include <stdint.h>
// #include "../../customlib/uart.c"

static void adc_init() {
    ADMUX = _BV(REFS0); // AVcc as reference
    ADCSRA = _BV(ADEN) | _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0); // enable ADC, prescaler 128
}

static uint16_t adc_read() {
    ADCSRA |= _BV(ADSC);        // start conversion
    while (ADCSRA & _BV(ADSC)); // wait for completion
    ADCSRA &= ~_BV(ADIF);       // clear interrupt flag
    return ADC;                 // 0..1023
}

// Timer0 as Fast PWM 8-bit non-inverting on OC0A (PD6)
static void pwm_init() {
    DDRD |= _BV(PD6);                               // OC0A as output
    TCCR0A = _BV(COM0A1) | _BV(WGM01) | _BV(WGM00); // non-inverting, Fast PWM 8-bit
    TCCR0B = _BV(CS01) | _BV(CS00);                 // prescaler 64
    OCR0A = 0;
}

int main() {
    adc_init();
    pwm_init();
    // uart_init();

    uint8_t smooth = 0;
    const uint8_t smoothing_factor = 8;

    while (1) {
        // take a few samples and average (reduces noise)
        const uint8_t num_samples = 6;
        uint32_t sum = 0;
        for (uint8_t i = 0; i < num_samples; ++i) {
            sum += adc_read();
            _delay_ms(5);
        }
        uint32_t v = (int32_t)(sum / num_samples); // 0..1023

        // nonlinear mapping (gamma ~2) for aesthetic brightness:
        unsigned long sq = (unsigned long)v * (unsigned long)v;
        uint8_t scaled = (uint8_t)((sq * 255u) / (1023u * 1023u));
        smooth = (uint8_t)((smooth * (smoothing_factor - 1) + scaled) / smoothing_factor);

        // printf("ADC: %lu -> PWM: %u (%u)\r\n", v, scaled, smooth);
        OCR0A = smooth;

        _delay_ms(40);
    }
}
