#include <avr/interrupt.h>
#include <avr/io.h>
#include <stdint.h>
#include <stdio.h>
#include <util/delay.h>

#include "../../customlib/uart.c"

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
    ADMUX = _BV(REFS0);                                 // AVcc jako referencja, ADC0 (PC0)
    DIDR0 = _BV(ADC0D) | _BV(ADC1D);                    // wyłącz wejścia cyfrowe na ADC0 i ADC1
    ADCSRA = _BV(ADEN) | _BV(ADPS2) | _BV(ADPS1);       // włącz ADC, preskaler 64 (125 kHz)
}

// odczyt z ADC0 (potencjometr)
static uint16_t adc_read() {
    ADMUX = _BV(REFS0);             // ADC0
    _delay_us(10);
    ADCSRA |= _BV(ADSC);            // rozpocznij konwersję
    while (ADCSRA & _BV(ADSC));     // czekaj na zakończenie
    ADCSRA |= _BV(ADIF);            // wyczyść flagę
    return ADC;                     // zwróć wartość 0..1023
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

    // włącz przerwania: CAPT (TOP) i OVF (BOTTOM)
    TIMSK1 = _BV(ICIE1) | _BV(TOIE1);
}

// odczyt z ADC1 (pomiary silnika)
static uint16_t adc1_read() {
    ADMUX = _BV(REFS0) | _BV(MUX0); // ADC1 (PC1)
    _delay_us(10);
    ADCSRA |= _BV(ADSC);
    while (ADCSRA & _BV(ADSC));
    ADCSRA |= _BV(ADIF);
    return ADC;
}

// przerwanie Input Capture (TOP) - tranzystor zamknięty
// mierzymy SEM silnika (napięcie 5V - V_measured proporcjonalne do prędkości)
ISR(TIMER1_CAPT_vect) {
    // ustaw ADC1 i uruchom pomiar
    ADMUX = _BV(REFS0) | _BV(MUX0); // ADC1
    _delay_us(10);
    ADCSRA |= _BV(ADSC);
    while (ADCSRA & _BV(ADSC));
    adc_at_top = ADC;
}

// przerwanie Overflow (BOTTOM) - tranzystor otwarty
// mierzymy napięcie na tranzystorze (proporcjonalne do prądu/momentu)
ISR(TIMER1_OVF_vect) {
    // ustaw ADC1 i uruchom pomiar
    ADMUX = _BV(REFS0) | _BV(MUX0); // ADC1
    ADCSRA |= _BV(ADSC);
    while (ADCSRA & _BV(ADSC));  // czekaj na wynik
    adc_at_bottom = ADC;
    adc_ready = 1;  // oba pomiary gotowe
}

int main() {
    uart_init();
    adc_init();
    pwm_init();

    sei();

    printf("Phase & Frequency Correct PWM Motor Control\r\n");
    printf("Potentiometer: ADC0 (PC0) | Motor: ADC1 (PC1)\r\n");
    printf("Measuring back-EMF (speed) and current (torque)\r\n\r\n");

    while (1) {
        // odczytaj potencjometr i ustaw wypełnienie PWM
        uint16_t pot_value = adc_read();
        uint16_t duty = (uint16_t)(((uint32_t)pot_value * (uint32_t)PWM_TOP) / 1023UL);
        OCR1A = duty;

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
        // gdy tranzystor otwarty, mierzymy spadek napięcia na Rds(on)
        uint32_t motor_current_mv = bottom_mv;

        printf("PWM: %4u/%u | EMF(speed): %4lu mV | I(torque): %4lu mV\r\n",
               duty, PWM_TOP, motor_emf_mv, motor_current_mv);

        _delay_ms(100);
    }
}
