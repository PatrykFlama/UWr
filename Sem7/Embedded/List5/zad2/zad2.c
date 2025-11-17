#include <avr/interrupt.h>
#include <avr/io.h>
#include <stdint.h>
#include <stdio.h>
#include <util/delay.h>

#include "../../customlib/uart.c"

// divider resistance (ohms)
#define R_PULL 10000UL
#define PRINT_INTERVAL_MS 1000

// last measured resistance in ohms (0..UINT32_MAX)
static volatile uint32_t last_ohms = 0;
static volatile uint16_t last_adc = 0;

// ADC0, AVcc reference, prescaler 128
static void adc_init() {
    // reference AVcc, ADC0
    ADMUX = _BV(REFS0) | (0 & 0x0F);

    // enable ADC, enable ADC interrupt, prescaler = 128
    ADCSRA = _BV(ADEN) | _BV(ADIE) | _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0);

    // disable auto-trigger (start conversion from INT0 ISR)
    ADCSRA &= ~_BV(ADATE);
}

// external interrupt (button) on INT0 (PD2)
static void int0_init() {
    // PD2 input with pull-up
    DDRD &= ~_BV(DDD2);
    PORTD |= _BV(PORTD2);

    // trigger on falling edge (button press to GND)
    EICRA |= _BV(ISC01);
    EICRA &= ~_BV(ISC00);

    // enable INT0
    EIMSK |= _BV(INT0);
}

// INT0 ISR: start ADC conversion
ISR(INT0_vect) {
    // start ADC conversion
    ADCSRA |= _BV(ADSC);
}

// ADC conversion complete ISR
ISR(ADC_vect) {
    uint16_t adc = ADC;  // read ADC
    last_adc = adc;

    if (adc == 0) {
        last_ohms = 0xFFFFFFFFUL;
    } else {
        // R_photo = R_PULL * (1023/adc - 1) = R_PULL * (1023 - adc) / adc
        uint32_t calc = (uint32_t)R_PULL * (uint32_t)(1023UL - adc);
        calc /= (uint32_t)adc;
        last_ohms = calc;
    }

    ADCSRA &= ~_BV(ADIF);
}

int main() {
    uart_init();

    adc_init();
    int0_init();

    // unmask interrupts
    sei();

    while (1) {
        uint16_t adc_val;
        uint32_t ohms_val;

        cli();
        adc_val = last_adc;
        ohms_val = last_ohms;
        sei();

        if (adc_val == 0) {
            // print as INF
            printf("R: INF (open)\r\n");
        } else {
            printf("R: %lu ohm (ADC=%u)\r\n", ohms_val, (unsigned)adc_val);
        }

        _delay_ms(PRINT_INTERVAL_MS);
    }
}
