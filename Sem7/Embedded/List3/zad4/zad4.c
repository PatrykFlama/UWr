/*
R_ntc = Rref * ADC / (1023 - ADC)
1/T = 1/T0 + (1/B) * ln(R/R0)  -> T w Kelvinach
*/

#include <avr/io.h>
#include <inttypes.h>
#include <math.h>
#include <util/delay.h>
#include "../../customlib/uart.c"

#define R_REF_OHM 10000.0  // our reference resistor value
#define R0_OHM 4700.0     // R0 in 25C (4.7k at 25C)
#define T0_K 273.15       // 0C in kelvins
#define B_CONST 4300.0    // B constant


void adc_init() {
    ADMUX = _BV(REFS0);
    DIDR0 = _BV(ADC0D);
    ADCSRA = _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0);
    ADCSRA |= _BV(ADEN);
}


static uint16_t adc_read_blocking() {
    ADCSRA |= _BV(ADSC);
    while (ADCSRA & _BV(ADSC));
    return ADC;
}

int main() {
    uart_init();
    adc_init();

    while (1) {
        uint16_t adc = adc_read_blocking();

        // calc thermistor resistance (viva la resistance!)
        double r_ntc = (R_REF_OHM * (double)adc) / (1023.0 - (double)adc);

        // calc temp in kelvin:
        // 1/T = 1/T0 + (1/B) * ln(R/R0)
        double ln_ratio = log(r_ntc / R0_OHM);
        double invT = (1.0 / (T0_K + 25)) + (ln_ratio / B_CONST);
        double T_K = 1.0 / invT;
        double T_C = T_K - 273.15;


        long temp_c_x100 = (long)(T_C * 100.0 + (T_C >= 0 ? 0.5 : -0.5));
        long t_int = temp_c_x100 / 100;
        // long t_frac = temp_c_x100 % 100;
        // t_frac = (t_frac < 0) ? -t_frac : t_frac;

        unsigned long r_rounded = (unsigned long)(r_ntc + 0.5);

        printf("ADC=%u  R= %lu ohm  T= %ld C\r\n", adc, r_rounded, t_int);

        _delay_ms(800);
    }
}
