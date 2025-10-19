#pragma once

#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/atomic.h>

// Calculate the value needed for the CTC match value in OCR1A (for 1ms)
#define CTC_MATCH_OVERFLOW ((F_CPU / 1000) / 64)

// https://gist.github.com/adnbr/2439125
volatile unsigned long timer_millis = 0;
ISR (TIMER1_COMPA_vect) {
    timer_millis++;
}

unsigned long millis(void) {
    unsigned long m;
    ATOMIC_BLOCK(ATOMIC_FORCEON) {
        m = timer_millis;
    }
    return m;
}

// Initialize Timer to generate interrupt every 1 ms (CTC mode)
static void timer_init_ms(void) {
    cli();
    
    // CTC mode, Clock/64 (WGM12 and CS11:CS10 = 1:1)
    TCCR1B |= (1 << WGM12) | (1 << CS11) | (1 << CS10);

    // Load OCR1A high and low bytes
    uint16_t ocr = (uint16_t)(CTC_MATCH_OVERFLOW - 1);
    OCR1AH = (uint8_t)(ocr >> 8);
    OCR1AL = (uint8_t)(ocr & 0xFF);

    // Enable the compare match interrupt
    TIMSK1 |= (1 << OCIE1A);

    sei();
}