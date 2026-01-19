#include <avr/io.h>
#include <stdint.h>
#include <stdio.h>
#include <util/delay.h>

#include "../../customlib/uart.c"

// L293D: EN1 -> PD3 (stałe HIGH), IN1 -> PB1 (OC1A), IN2 -> PB2 (OC1B)
// PWM: Fast PWM, TOP=ICR1, f ~ 1 kHz
// F_PWM = F_CPU / (prescaler * (1 + TOP)) = 16 MHz / (8 * 2000) = 1 kHz
#define PWM_TOP 1999
#define DEAD_BAND 8  // środkowy odcinek potencjometru = STOP

// ADC0 (PC0) do potencjometru
static void adc_init() {
	ADMUX = _BV(REFS0);                                 // AVcc jako referencja, kanał ADC0
	DIDR0 = _BV(ADC0D);                                 // wyłącz wejście cyfrowe na ADC0
	ADCSRA = _BV(ADEN) | _BV(ADPS2) | _BV(ADPS1);       // włącz ADC, preskaler 64 (125 kHz)
}

static uint16_t adc_read() {
	ADCSRA |= _BV(ADSC);
	while (ADCSRA & _BV(ADSC))
		;
	ADCSRA |= _BV(ADIF);
	return ADC;  // 0..1023
}

// Timer1: Fast PWM, non-inverting na OC1A/B, preskaler 8, TOP=ICR1
static void pwm_init() {
	// OC1A (PB1), OC1B (PB2)
	DDRB |= _BV(PB1) | _BV(PB2);

	ICR1 = PWM_TOP;
	TCCR1A = _BV(COM1A1) | _BV(COM1B1) | _BV(WGM11);
	TCCR1B = _BV(WGM13) | _BV(WGM12) | _BV(CS11);  // preskaler 8

	OCR1A = 0;
	OCR1B = 0;
}

// EN1 pin dla L293D ustawiony na stałe HIGH
static void l293d_enable() {
	DDRD |= _BV(PD3);
	PORTD |= _BV(PD3);
}
static void l293d_disable() {
    DDRD |= _BV(PD3);
    PORTD &= ~_BV(PD3);
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

int main() {
	uart_init();
	adc_init();
	pwm_init();
	l293d_enable();

	printf("L293D Motor Control - Bidirectional\r\n");
	printf("ADC center: 512, deadband: +/-%d\r\n\r\n", DEAD_BAND);

	while (1) {
		uint16_t adc_val = adc_read();
		const char* direction;
		uint16_t duty;

		// kierunek i prędkość
		if (adc_val + DEAD_BAND < 512) {
			// lewo / reverse: IN2 = PWM, IN1 = 0
			duty = map_speed(adc_val);
			OCR1A = 0;
			OCR1B = duty;
			direction = "REVERSE";
		} else if (adc_val > 512 + DEAD_BAND) {
			// prawo / forward: IN1 = PWM, IN2 = 0
			duty = map_speed(adc_val);
			OCR1A = duty;
			OCR1B = 0;
			direction = "FORWARD";
		} else {
			// martwa strefa = STOP
			duty = 0;
			OCR1A = 0;
			OCR1B = 0;
			direction = "STOP";
		}

		printf("ADC: %4u | Dir: %-7s | PWM: %4u/%u | OCR1A: %4u | OCR1B: %4u\r\n",
		       adc_val, direction, duty, PWM_TOP, OCR1A, OCR1B);

		_delay_ms(200);
	}
}
