#include <avr/io.h>
#include <stdint.h>
#include <util/delay.h>
#include "../../customlib/uart.c"


#define DETECTOR_FREQ 37900UL
#define BURST_US 600U
#define BURST_COUNT 6U
#define INTER_BURST_US 600U
#define SEQUENCE_GAP_MS 100U

// compute ICR1 top for desired carrier using prescaler = 1
#define ICR1_TOP ((uint16_t)(F_CPU / DETECTOR_FREQ - 1UL))

static void init_diode() {
    // Fast PWM, TOP = ICR1 (mode 14): WGM13:0 = 14 -> WGM13=1,WGM12=1,WGM11=1,WGM10=0
    ICR1 = ICR1_TOP;
    // disconnect OC1A
    TCCR1A = _BV(WGM11);
    // WGM13 and WGM12 + prescaler = 1
    TCCR1B = _BV(WGM13) | _BV(WGM12) | _BV(CS10);
    // half duty cycle
    OCR1A = ICR1_TOP / 2;
    // ensure PB1 as output
    DDRB |= _BV(PB1);
    // disable output initially
    TCCR1A &= ~(_BV(COM1A1) | _BV(COM1A0));
    PORTB |= _BV(PB1);
}

// enable OC1A output in non-inverting PWM (connect PWM to PB1)
static inline void diode_enable() {
    // inverting mode: COM1A1=1, COM1A0=1
    TCCR1A |=  _BV(COM1A1) | _BV(COM1A0);
}

// disable OC1A output (disconnect OC1A pin from timer)
static inline void diode_disable() {
    // disconnect OC1A
    TCCR1A &= ~(_BV(COM1A1) | _BV(COM1A0));
    // force pin high
    PORTB |= _BV(PB1);
}

static void init_detector() {
    // IR detector active low output
    DDRB &= ~_BV(PB0);
    PORTB |= _BV(PB0);

    // LED on PB5
    DDRB |= _BV(PB5);
    PORTB &= ~_BV(PB5);
}


int main() {
    init_diode();
    init_detector();
    uart_init();

    while (1) {
        for (uint8_t i = 0; i < BURST_COUNT; ++i) {
            diode_enable();
            _delay_us(BURST_US);

            diode_disable();
            _delay_us(INTER_BURST_US);

            // voltage low if sequence detected
            uint8_t rec = PINB & _BV(PB0);
            if (rec) {
                printf("IR not detected during burst %u\r\n", i + 1);
                PORTB &= ~_BV(PB5);
            } else {
                printf("IR detected during burst %u\r\n", i + 1);
                PORTB |= _BV(PB5);
            }
        }

        diode_disable();

        _delay_ms(SEQUENCE_GAP_MS);
    }
}
