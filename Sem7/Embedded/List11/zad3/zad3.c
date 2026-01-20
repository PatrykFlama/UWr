#include <avr/io.h>
#include <stdint.h>
#include <stdio.h>
#include <util/delay.h>

#include "../../customlib/uart.c"

// PWM: Fast PWM, TOP=ICR1, f ~ 1 kHz
// F_PWM = F_CPU / (prescaler * (1 + TOP)) = 16 MHz / (8 * 2000) = 1 kHz
#define PWM_TOP 1999
#define DEAD_BAND 8  // środkowy odcinek potencjometru = STOP

#define PIN1 PD5
#define PIN2 PD4

// ADC0 (PC0) do potencjometru
static void adc_init() {
    ADMUX = _BV(REFS0);                                 // AVcc as ref, ADC0 (PC0)
	DIDR0 = _BV(ADC0D);                                 // disable digital input on ADC0
    ADCSRA = _BV(ADEN) | _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0);  // enable ADC, prescaler 128 (125 kHz)
}

static uint16_t adc_read() {
	ADCSRA |= _BV(ADSC);
	while (ADCSRA & _BV(ADSC));
	ADCSRA |= _BV(ADIF);
	return ADC;  // 0..1023
}

// Timer1: Fast PWM, non-inverting na OC1A/B, preskaler 8, TOP=ICR1
static void pwm_init() {
	// OC1A (PB1)
	DDRB |= _BV(PB1);

	ICR1 = PWM_TOP;
	TCCR1A = _BV(COM1A1) | _BV(WGM11);
	TCCR1B = _BV(WGM13) | _BV(WGM12) | _BV(CS11);  // preskaler 8

	OCR1A = 0;
}

static uint16_t map_speed(uint16_t adc_val) {
	// środek ~512, zakres 0..1023; wynik 0..PWM_TOP dla wartości dodatnich
	if (adc_val + DEAD_BAND < 512) {
		uint16_t mag = (uint16_t)(512 - adc_val);
		return (uint16_t)(((uint32_t)mag * PWM_TOP) / 511UL);
	} else if (adc_val > 512 + DEAD_BAND) {
		uint16_t mag = (uint16_t)(adc_val - 512);
		return (uint16_t)(((uint32_t)mag * PWM_TOP) / 511UL);
	}
	return 0;
}

static void direction_right() {
	PORTD |= _BV(PIN1);
	PORTD &= ~_BV(PIN2);
}
static void direction_left() {
	PORTD &= ~_BV(PIN1);
	PORTD |= _BV(PIN2);
}

int main() {
	uart_init();
	adc_init();
	pwm_init();

	DDRD |= _BV(PIN1) | _BV(PIN2);

	printf("L293D Motor Control - Bidirectional\r\n");
	printf("ADC center: 512, deadband: +/-%d\r\n\r\n", DEAD_BAND);

	while (1) {
		uint16_t adc_val = adc_read();
		const char* direction;
		uint16_t duty;

		// kierunek i prędkość
		if (adc_val + DEAD_BAND < 512) {
			duty = map_speed(adc_val);
			direction_right();
			OCR1A = duty;
			direction = "REVERSE";
		} else if (adc_val > 512 + DEAD_BAND) {
			duty = map_speed(adc_val);
			direction_left();
			OCR1A = duty;
			direction = "FORWARD";
		} else {
			OCR1A = 0;
			direction = "STOP";
		}

		printf("ADC: %4u | Dir: %-7s | PWM: %4u/%u | OCR1A: %4u | PIN1: %1u | PIN2: %1u\n\r",
		       adc_val, direction, duty, PWM_TOP, OCR1A, PORTD&_BV(PIN1), PORTD&_BV(PIN2));

		_delay_ms(200);
	}
}
