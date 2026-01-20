#include <avr/io.h>
#include <stdint.h>
#include <stdio.h>
#include <util/delay.h>

#include "../../customlib/uart.c"

// Sterowanie serwem modelarskim
// Częstotliwość PWM: 50 Hz (okres 20 ms)
// F_PWM = F_CPU / (prescaler * (1 + TOP))
// 50 = 16000000 / (8 * (1 + TOP))
// TOP = 39999
#define PWM_TOP 39999
#define PWM_PRESCALER 8

#define SERVO_MIN 1500
#define SERVO_MAX 5000

// ADC na kanale ADC0 (PC0) - potencjometr
static void adc_init() {
    ADMUX = _BV(REFS0);                                 // AVcc as ref, ADC0 (PC0)
    DIDR0 = _BV(ADC0D);                                 // disable digital input on ADC0
    ADCSRA = _BV(ADEN) | _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0);  // enable ADC, prescaler 128 (125 kHz)
}

static uint16_t adc_read() {
    ADCSRA |= _BV(ADSC);
    while (ADCSRA & _BV(ADSC));
    ADCSRA |= _BV(ADIF);
    return ADC;
}

// Timer1: Fast PWM, non-inverting na OC1B (PB2), 50 Hz
static void pwm_init() {
    // ustaw PB2 (OC1B) jako wyjście
    DDRB |= _BV(PB2);
    
    // Fast PWM, TOP=ICR1, non-inverting mode
    // COM1B = 10   -- non-inverting
    // WGM1  = 1110 -- Fast PWM, TOP=ICR1
    // CS1   = 010  -- preskaler 8
    ICR1 = PWM_TOP;
    TCCR1A = _BV(COM1B1) | _BV(WGM11);
    TCCR1B = _BV(WGM13) | _BV(WGM12) | _BV(CS11);
    
    // początkowa pozycja środkowa
    OCR1B = (SERVO_MIN + SERVO_MAX) / 2;
}

int main() {
    uart_init();
    adc_init();
    pwm_init();
    
    printf("Servo Control - 50 Hz PWM\r\n");
    printf("Safe range: %u - %u (%.2f ms - %.2f ms)\r\n\r\n",
           SERVO_MIN, SERVO_MAX,
           (float)SERVO_MIN * 20.0f / (float)PWM_TOP,
           (float)SERVO_MAX * 20.0f / (float)PWM_TOP);
    
    while (1) {
        // odczytaj potencjometr (0..1023)
        uint16_t adc_value = adc_read();
        
        // przeskaluj na zakres serwa (SERVO_MIN..SERVO_MAX)
        uint16_t servo_pos = SERVO_MIN + 
                             (uint16_t)(((uint32_t)adc_value * (uint32_t)(SERVO_MAX - SERVO_MIN)) / 1023UL);
        
        // ustaw pozycję serwa
        OCR1B = servo_pos;
        
        // wyświetl informacje diagnostyczne
        float pulse_ms = ((float)servo_pos * 20.0f) / (float)PWM_TOP;
        int pulse_int = (int)(pulse_ms * 100.0f);
        printf("ADC: %4u | Servo: %4u | Pulse: %u e-2 ms\r\n", 
               adc_value, servo_pos, pulse_int);
        
        _delay_ms(50);
    }
}
