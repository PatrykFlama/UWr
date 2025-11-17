#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/sleep.h>
#include <stdint.h>

// Sampling period: 10 ms -> 100 Hz
// Timer1 CTC: prescaler 64 -> OCR1A = F_CPU/(prescaler*freq) - 1
#define TIMER1_PRESCALER_BITS ((1 << CS11) | (1 << CS10))  // clk/64 (CS12=0,CS11=1,CS10=1)
#define SAMPLE_FREQ_HZ 100
#define OCR1A_VALUE ((F_CPU / (64UL * SAMPLE_FREQ_HZ)) - 1)

// Buffer length = number of samples for 1 second delay
#define BUFFER_LEN 100

// circular buffer stored as bytes (0/1)
static volatile uint8_t buffer[BUFFER_LEN];
static volatile uint8_t buf_index = 0;

// initialize IO: PD2 input with pull-up, PB5 output
static void io_init() {
    // PD2 input (button), enable pull-up
    DDRD &= ~_BV(DDD2);
    PORTD |= _BV(PORTD2);

    // PB5 output (LED)
    DDRB |= _BV(DDB5);
    PORTB &= ~_BV(PORTB5);
}

// initialize Timer1 in CTC mode to generate ISR every 10 ms
static void timer1_init() {
    // CTC mode: WGM12 = 1
    TCCR1B |= _BV(WGM12);
    // set compare value
    OCR1A = (uint16_t)OCR1A_VALUE;
    // enable Output Compare A Match interrupt
    TIMSK1 |= _BV(OCIE1A);
    // set prescaler to clk/64 (CS12:0 = 0b011)
    TCCR1B |= (_BV(CS11) | _BV(CS10));
}

// Timer1 Compare A interrupt - executed every 10 ms
ISR(TIMER1_COMPA_vect) {
    // read button (active low)
    uint8_t pressed = !(PIND & _BV(PIND2));

    // store current sample into buffer at buf_index
    buffer[buf_index] = pressed ? 1 : 0;

    // compute index of sample from 1 second ago
    uint8_t delayed_index = (uint8_t)(buf_index + 1);
    if (delayed_index >= BUFFER_LEN) delayed_index -= BUFFER_LEN;

    // set LED according to delayed sample: LED on when sample==1
    if (buffer[delayed_index])
        PORTB |= _BV(PORTB5);
    else
        PORTB &= ~_BV(PORTB5);

    // advance index
    buf_index++;
    if (buf_index >= BUFFER_LEN) buf_index = 0;
}

int main() {
    // zero buffer
    for (uint8_t i = 0; i < BUFFER_LEN; ++i) buffer[i] = 0;

    io_init();
    cli();
    timer1_init();

    // configure sleep mode and allow sleeping in main loop
    set_sleep_mode(SLEEP_MODE_IDLE);

    // unmask interrupts
    sei();

    while (1) {
        sleep_mode();
    }
}
