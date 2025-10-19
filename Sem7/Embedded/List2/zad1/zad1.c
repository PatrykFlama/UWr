#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/atomic.h>

// Calculate the value needed for the CTC match value in OCR1A (for 1ms)
#define CTC_MATCH_OVERFLOW ((F_CPU / 1000) / 64)

// https://gist.github.com/adnbr/2439125
volatile unsigned long timer_millis = 0;
ISR (TIMER1_COMPA_vect)
{
    timer_millis++;
}

unsigned long millis(void)
{
    unsigned long m;
    ATOMIC_BLOCK(ATOMIC_FORCEON) {
        m = timer_millis;
    }
    return m;
}

// Initialize Timer to generate interrupt every 1 ms (CTC mode)
static void timer_init_ms(void)
{
    // CTC mode, Clock/64 (WGM12 and CS11:CS10 = 1:1)
    TCCR1B |= (1 << WGM12) | (1 << CS11) | (1 << CS10);

    // Load OCR1A high and low bytes
    uint16_t ocr = (uint16_t)(CTC_MATCH_OVERFLOW - 1);
    OCR1AH = (uint8_t)(ocr >> 8);
    OCR1AL = (uint8_t)(ocr & 0xFF);

    // Enable the compare match interrupt
    TIMSK1 |= (1 << OCIE1A);
}

#define LED PB5
#define LED_DDR DDRB
#define LED_PORT PORTB

#define BTN PB4
#define BTN_PIN PINB
#define BTN_PORT PORTB

#define BUFOR_SIZE 16
#define TIME_OFFSET 1000   // milliseconds

struct State
{
    uint8_t btn_state; // 0 or 1
    unsigned long timestamp; // millis when to play this event
};

int bufor_begin = 0; // index of oldest event
int bufor_end = 0;   // index one past last event (empty when equal to begin)
struct State bufor[BUFOR_SIZE];


int main() {
    // enable pull-up on button pin, set LED as output
    BTN_PORT |= _BV(BTN);
    LED_DDR |= _BV(LED);

    // initialize timer
    cli();
    timer_init_ms();
    sei();

    uint8_t last_state = (BTN_PIN & _BV(BTN)) ? 1 : 0;

    while (1) {
        // register button change (polling)
        const uint8_t curr_state = (BTN_PIN & _BV(BTN)) ? 1 : 0;
        if (curr_state != last_state) {
            last_state = curr_state;

            unsigned long play_time = timer_millis + TIME_OFFSET;

            int next_end = (bufor_end + 1) % BUFOR_SIZE;
            // if buffer full, drop oldest (advance begin)
            if (next_end == bufor_begin) {
                bufor_begin = (bufor_begin + 1) % BUFOR_SIZE;
            }

            bufor[bufor_end].btn_state = curr_state;
            bufor[bufor_end].timestamp = play_time;
            bufor_end = next_end;
        }

        // replay events when their timestamp has passed
        if (bufor_begin != bufor_end) {
            unsigned long ts;
            ATOMIC_BLOCK(ATOMIC_RESTORESTATE) {
                ts = bufor[bufor_begin].timestamp;
            }
            if (timer_millis >= ts) {
                if (bufor[bufor_begin].btn_state)
                    LED_PORT &= ~_BV(LED);
                else
                    LED_PORT |= _BV(LED);

                bufor_begin = (bufor_begin + 1) % BUFOR_SIZE;
            }
        }
    }
}

