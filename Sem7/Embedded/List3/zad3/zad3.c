#include <avr/io.h>
#include <util/delay.h>
#include <inttypes.h>

#define LED_PIN PB5
#define LED_DDR DDRB
#define LED_PORT PORTB

// PWM period (~500 Hz)
#define PERIOD_US 2000UL
// expected ADC conversion time
#define ADC_CONV_US 130UL

static inline void delay_us(uint32_t us) {
    while (us--) _delay_us(1);
}

static void adc_init() {
    // AVcc as reference, ADC0 (MUX=0) selected initially
    ADMUX = _BV(REFS0);
    // disable digital input on ADC0
    DIDR0 = _BV(ADC0D);
    // prescaler 128 -> ADC clock ~125kHz, enable ADC
    ADCSRA = _BV(ADEN) | _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0);
}

static uint16_t adc_read_blocking() {
    ADCSRA |= _BV(ADSC);         // start conversion
    while (ADCSRA & _BV(ADSC));  // wait
    ADCSRA |= _BV(ADIF);         // clear start conversion flag
    return ADC;
}

int main() {
    // configure LED output
    LED_DDR |= _BV(LED_PIN);
    LED_PORT &= ~_BV(LED_PIN);

    adc_init();

    while (1) {
        static uint16_t adc = 512;

        // perceptual correction: approximate gamma by squaring input
        // uint32_t corr = ((uint32_t)adc * (uint32_t)adc) >> 12;
        uint32_t corr = (((uint32_t)adc << 2) * 1024) >> 2;

        // calculate on time in microseconds
        uint32_t on_us = (corr * PERIOD_US) / 1023UL;

        // ensure room for ADC conversion during OFF phase
        if (on_us > PERIOD_US) on_us = PERIOD_US;
        if (on_us > PERIOD_US - ADC_CONV_US)
            on_us = (PERIOD_US > ADC_CONV_US) ? (PERIOD_US - ADC_CONV_US) : 0;

        // ON phase
        if (on_us) {
            LED_PORT |= _BV(LED_PIN);
            delay_us(on_us);
        }

        // OFF phase
        LED_PORT &= ~_BV(LED_PIN);

        adc = adc_read_blocking();
    }
}
