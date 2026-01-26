#include <avr/interrupt.h>
#include <avr/io.h>
#include <stdint.h>
#include <stdio.h>
#include <util/delay.h>

#include "../../customlib/uart.c"
#include "../AVR221/IAR/pid.h"

// Konfiguracja Phase and Frequency Correct PWM na Timer1
// Częstotliwość PWM: ~1 kHz (dwukierunkowy licznik)
// F_PWM = F_CPU / (2 * prescaler * TOP)
// 1000 = 16000000 / (2 * 8 * TOP)
// TOP = 1000
#define PWM_PRESCALER 8
#define PWM_TOP 1000

// AVcc = 5000 mV
#define VREF_MV 5000UL

// wyniki ADC
static volatile uint16_t adc_at_top = 0;     // pomiar przy TOP (tranzystor zamknięty)
static volatile uint16_t adc_at_bottom = 0;  // pomiar przy BOTTOM (tranzystor otwarty)
static volatile uint8_t adc_ready = 0;       // flaga gotowych pomiarów

// inicjalizacja ADC na kanale ADC0 (PC0) do odczytu potencjometru
static void adc_init() {
    ADMUX = _BV(REFS0);                                 // AVcc as ref, ADC0 (PC0)
    DIDR0 = _BV(ADC0D) | _BV(ADC1D);                    // disable digital input on ADC0 and ADC1
    ADCSRA = _BV(ADEN) | _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0);  // enable ADC, prescaler 128 (125 kHz)
}

// odczyt z ADC0 (potencjometr)
static uint16_t adc0_read() {
    ADMUX = _BV(REFS0); // ADC0 (PC0)
    ADCSRA |= _BV(ADSC);
    while (ADCSRA & _BV(ADSC));
    ADCSRA |= _BV(ADIF);
    return ADC;
}

// inicjalizacja Timer1 jako Phase and Frequency Correct PWM
static void pwm_init() {
    // ustaw PB1 (OC1A) jako wyjście
    DDRB |= _BV(PB1);

    // Phase and Frequency Correct PWM, TOP=ICR1, non-inverting mode
    // COM1A = 10   -- non-inverting (clear on up-counting, set on down-counting)
    // WGM1  = 1000 -- Phase and Frequency Correct PWM, TOP=ICR1
    // CS1   = 010  -- preskaler 8
    ICR1 = PWM_TOP;
    TCCR1A = _BV(COM1A1);
    TCCR1B = _BV(WGM13) | _BV(CS11);

    // początkowe wypełnienie 0 - silnik zatrzymany
    OCR1A = 0;

    // przerwania: CAPT (TOP) i OVF (BOTTOM)
    TIMSK1 = _BV(ICIE1) | _BV(TOIE1);
}

// odczyt z ADC1 (pomiary silnika)
static uint16_t adc1_read() {
    ADMUX = _BV(REFS0) | _BV(MUX0); // ADC1 (PC1)
    ADCSRA |= _BV(ADSC);
    while (ADCSRA & _BV(ADSC));
    ADCSRA |= _BV(ADIF);
    return ADC;
}

// przerwanie Input Capture (TOP) - tranzystor zamknięty
// mierzymy napięcie na silniku (back-EMF)
ISR(TIMER1_CAPT_vect) {
    adc_at_top = adc1_read();
}

// przerwanie Overflow (BOTTOM) - tranzystor otwarty
// mierzymy napięcie na tranzystorze (proporcjonalne do prądu/momentu)
ISR(TIMER1_OVF_vect) {
    adc_at_bottom = adc1_read();
    adc_ready = 1;  // oba pomiary gotowe
}

int main() {
    uart_init();
    adc_init();
    pwm_init();

    pidData_t pid_data;
    pid_Init(8, 1, 10, &pid_data);

    sei();

    while (1) {
        // potencjometr jako target speed (0-1023)
        uint16_t pot_value = adc0_read();
        int16_t target_speed = (int16_t)pot_value;

        while (!adc_ready);

        cli();
        uint16_t top_adc = adc_at_top;
        uint16_t bottom_adc = adc_at_bottom;
        adc_ready = 0;
        sei();

        // ADC = 0..1023 -> 0..5000 mV
        uint32_t top_mv = ((uint32_t)top_adc * VREF_MV) / 1023UL;
        uint32_t bottom_mv = ((uint32_t)bottom_adc * VREF_MV) / 1023UL;

        // SEM silnika: napięcie na silniku = 5V - V_measured
        // gdy tranzystor zamknięty (TOP), mierzymy napięcie między silnikiem a masą
        uint32_t motor_emf_mv = VREF_MV - top_mv;

        // Prąd silnika: napięcie na tranzystorze (BOTTOM)
        uint32_t motor_current_mv = bottom_mv;

        // motor_emf_mv (0-5000mV) na current_speed (0-1023)
        int16_t current_speed = (int16_t)((motor_emf_mv * 1023UL) / VREF_MV);
        if (current_speed < 0) current_speed = 0;
        if (current_speed > 1023) current_speed = 1023;

        // Oblicz wyjście PID
        int16_t pid_output = pid_Controller(target_speed, current_speed, &pid_data);

        // pid_output na duty (0 do PWM_TOP)
        uint16_t duty = 0;
        if (pid_output > 0) {
            duty = (uint16_t)pid_output;
            if (duty > PWM_TOP) duty = PWM_TOP;
        }

        OCR1A = duty;

        printf("PWM:%4u/%u|EMF:%4lu mV|Cur:%4d|Tgt:%4d|PID:%5d\r",
               duty, PWM_TOP, motor_emf_mv, current_speed, target_speed, pid_output);
        fflush(stdout);

        _delay_ms(100);
    }
}
