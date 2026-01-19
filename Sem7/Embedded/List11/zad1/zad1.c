#include <avr/io.h>
#include <stdint.h>
#include <util/delay.h>

// PWM na Timer1, OC1A (PB1)
// 1 kHz
// F_PWM = F_CPU / (prescaler * (1 + TOP))
// 1000 = 16000000 / (8 * (1 + TOP))
// TOP = 1999
#define PWM_PRESCALER 8
#define PWM_TOP 1999
#define PWM_FREQ_HZ 1000

// ADC na ADC0 (PC0)
static void adc_init() {
    ADMUX = _BV(REFS0);                                 // AVcc as ref, ADC0 (PC0)
    DIDR0 = _BV(ADC0D);                                 // disable digital input on ADC0
    ADCSRA = _BV(ADEN) | _BV(ADPS2) | _BV(ADPS1);       // enable ADC, prescaler 64 (125 kHz)
}

static uint16_t adc_read() {
    ADCSRA |= _BV(ADSC);
    while (ADCSRA & _BV(ADSC));
    ADCSRA |= _BV(ADIF);
    return ADC;
}

// Timer1 as Fast PWM, non-inverting on OC1A (PB1)
static void pwm_init() {
    // ustaw PB1 (OC1A) jako wyjście
    DDRB |= _BV(PB1);
    
    // Fast PWM, TOP=ICR1, non-inverting mode
    // COM1A = 10   -- non-inverting (clear on compare match, set at BOTTOM)
    // WGM1  = 1110 -- Fast PWM, TOP=ICR1
    // CS1   = 010  -- preskaler 8
    ICR1 = PWM_TOP;
    TCCR1A = _BV(COM1A1) | _BV(WGM11);
    TCCR1B = _BV(WGM13) | _BV(WGM12) | _BV(CS11);
    
    // początkowe wypełnienie 0 - silnik zatrzymany
    OCR1A = 0;
}

int main() {
    adc_init();
    pwm_init();
    
    while (1) {
        uint16_t adc_value = adc_read();
        
        // przeskaluj na zakres PWM (0..PWM_TOP)
        // OCR1A = (adc_value * PWM_TOP) / 1023
        uint16_t duty = (uint16_t)(((uint32_t)adc_value * (uint32_t)PWM_TOP) / 1023UL);
        
        // ustaw wypełnienie PWM
        OCR1A = duty;
        
        _delay_ms(10);
    }
}
