#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/sleep.h>
#include <stdint.h>
#include <stdio.h>
#include <util/delay.h>

#include "../../customlib/uart.c"

#define SAMPLES 500

static volatile uint16_t adc_result = 0;

ISR(ADC_vect) {
    adc_result = ADC;
}

static void adc_config() {
    // Disable Analog Comparator
    ACSR |= _BV(ACD);

    // AVcc (Vcc) as ADC reference: REFS1:0 = 0b01
    // MUX bits = 0b1110 -> internal 1.1V reference
    ADMUX = _BV(REFS0) | (14 & 0x0F);
    // prescaler = 128
    ADCSRA = _BV(ADEN) | _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0);
    // ADC interrupt disabled
    ADCSRA &= ~_BV(ADIE);
}

// perform N conversions using active polling
static void measure_active(uint32_t* out_sum, uint64_t* out_sumsq) {
    uint32_t sum = 0;
    uint64_t sumsq = 0;

    // disable ADC interrupt
    ADCSRA &= ~_BV(ADIE);

    // ignore first few
    for (int d = 0; d < 4; ++d) {
        ADCSRA |= _BV(ADSC);
        while (ADCSRA & _BV(ADSC));
        ADCSRA &= ~_BV(ADIF);
    }

    for (uint16_t i = 0; i < SAMPLES; ++i) {
        // start conversion
        ADCSRA |= _BV(ADSC);
        // wait until conversion finished
        while (ADCSRA & _BV(ADSC));
        ADCSRA &= ~_BV(ADIF);

        uint16_t v = ADC;
        sum += v;
        sumsq += (uint64_t)v * (uint64_t)v;
    }

    *out_sum = sum;
    *out_sumsq = sumsq;
}

// perform N conversions using ADC Noise Reduction (sleep in ADC mode)
static void measure_noise_reduction(uint32_t* out_sum, uint64_t* out_sumsq) {
    uint32_t sum = 0;
    uint64_t sumsq = 0;

    // enable ADC interrupt
    ADCSRA |= _BV(ADIE);

    // set sleep mode to ADC Noise Reduction
    set_sleep_mode(SLEEP_MODE_ADC);

    for (uint16_t i = 0; i < SAMPLES; ++i) {
        // start conversion
        adc_result = 0;
        ADCSRA |= _BV(ADSC);

        // sleep until ADC_vect wakes us
        sleep_mode();
        ADCSRA &= ~_BV(ADIF);

        uint16_t v = adc_result;
        sum += v;
        sumsq += (uint64_t)v * (uint64_t)v;
    }

    // disable ADC interrupt
    ADCSRA &= ~_BV(ADIE);

    *out_sum = sum;
    *out_sumsq = sumsq;
}

int main() {
    uart_init();
    adc_config();

    _delay_ms(50);

    sei();

    // ignore first result
    ADCSRA |= _BV(ADSC);
    while (ADCSRA & _BV(ADSC));
    ADCSRA &= ~_BV(ADIF);


    uint32_t sum_active;
    uint64_t sumsq_active;
    uint32_t sum_noise;
    uint64_t sumsq_noise;

    measure_active(&sum_active, &sumsq_active);
    _delay_ms(200);
    measure_noise_reduction(&sum_noise, &sumsq_noise);


    // compute mean and variance (variance in ADC units^2)
    uint32_t mean_active = sum_active / SAMPLES;
    uint32_t mean_noise = sum_noise / SAMPLES;

    uint64_t var_active = (sumsq_active / SAMPLES) - (uint64_t)mean_active * (uint64_t)mean_active;
    uint64_t var_noise = (sumsq_noise / SAMPLES) - (uint64_t)mean_noise * (uint64_t)mean_noise;

    printf("Active: mean=%lu var=%lu\r\n", (unsigned long)mean_active, (unsigned long)var_active);
    printf("NoiseR: mean=%lu var=%lu\r\n", (unsigned long)mean_noise, (unsigned long)var_noise);

    // printf("Raw active: sum=%lu sumsq=%lu\r\n", (unsigned long)sum_active, (unsigned long)sumsq_active);
    // printf("Raw noise:  sum=%lu sumsq=%lu\r\n", (unsigned long)sum_noise, (unsigned long)sumsq_noise);
}
