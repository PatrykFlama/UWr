#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/sleep.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <util/delay.h>

#include "../../customlib/uart.c"

// Timer1 prescaler value (must match CS1x bits: here CS11 -> prescaler 8)
#define T1_PRESCALER 8UL

// Measurement state (shared with ISRs)
volatile uint32_t t_overflow_count = 0;  // counts Timer1 overflows
volatile uint32_t t_last_total = 0;      // last 32-bit time (overflows<<16 | ICR1)
volatile uint8_t t_have_last = 0;        // whether we have a previous capture
volatile uint32_t measured_freq = 0;     // last measured frequency [Hz]
volatile uint8_t new_measurement = 0;    // flag set by ISR when a new measurement ready

// Timer1 overflow ISR
ISR(TIMER1_OVF_vect) {
    t_overflow_count++;
}

// Input Capture ISR (edge on ICP1)
ISR(TIMER1_CAPT_vect) {
    // read capture value and current overflow count
    uint16_t icr = ICR1;
    uint32_t ov = t_overflow_count;
    // compose 32-bit timestamp
    uint32_t total = (ov << 16) | icr;

    if (t_have_last) {
        uint32_t delta = total - t_last_total;
        if (delta != 0) {
            // frequency = (F_CPU / prescaler) / delta_ticks
            measured_freq = (uint32_t)((F_CPU / T1_PRESCALER) / (uint32_t)delta);
            new_measurement = 1;
        }
    } else {
        t_have_last = 1;
    }

    t_last_total = total;
}

void timer_config() {
    TCCR1A = 0;
    // noise canceler, capture on rising edge, prescaler 8
    TCCR1B = _BV(ICNC1) | _BV(ICES1) | _BV(CS11);
    // enable Input Capture,  Overflow interrupts
    TIMSK1 = _BV(ICIE1) | _BV(TOIE1);
}

int main() {
    // char buf[32];
    uart_init();

    timer_config();

    sei();

    // idle sleep mode - Timer1 and UART kept running
    set_sleep_mode(SLEEP_MODE_IDLE);

    while (1) {
        sleep_mode();

        if (new_measurement) {
            cli();
            uint32_t freq = measured_freq;
            new_measurement = 0;
            sei();

            printf("Frequency: %lu Hz\r\n", freq);
        }
    }
}
