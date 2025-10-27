#include <avr/io.h>
#include <inttypes.h>
#include <stdio.h>
#include <util/delay.h>

#include "../../customlib/uart.c"

#define LED_PIN PB5
#define LED_DDR DDRB
#define LED_PORT PORTB

void adc_init() {
    // Select Vref = AVcc, MUX = 1110 (internal 1.1V)
    ADMUX = _BV(REFS0) | _BV(MUX3) | _BV(MUX2) | _BV(MUX1);
    DIDR0 = 0;  // disable digital input on all ADC pins

    // Enable ADC, prescaler 128
    ADCSRA = _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0);
    ADCSRA |= _BV(ADEN);
}

// measure internal 1.1V bandgap with AVcc as reference
static uint16_t read_internal_1v1_adc() {
    // conversion
    ADCSRA |= _BV(ADSC);
    while (ADCSRA & _BV(ADSC));

    return ADC;
}

int main() {
    uart_init();
    adc_init();

    // configure LED pin
    LED_DDR |= _BV(LED_PIN);
    LED_PORT &= ~_BV(LED_PIN);

    while (1) {
        uint16_t adc = read_internal_1v1_adc();
        // Vcc (mV) = 1.1V * 1023 * 1000 / ADC
        unsigned long vcc_mV = 1125300UL / adc;  // 1.1 * 1023 * 1000 = 1125300
        unsigned long v = vcc_mV / 1000;
        unsigned long mv = vcc_mV % 1000;
        printf("Vcc = %lu.%03luv\r\n", v, mv);

        // toggle LED between measurements
        _delay_ms(500);
        LED_PORT ^= _BV(LED_PIN);

        _delay_ms(500);
    }
}
