#include <avr/io.h>
#include "../../customlib/millis.c"

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
        // register button change
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
            if (timer_millis >= bufor[bufor_begin].timestamp) {
                if (bufor[bufor_begin].btn_state)
                    LED_PORT &= ~_BV(LED);
                else
                    LED_PORT |= _BV(LED);

                bufor_begin = (bufor_begin + 1) % BUFOR_SIZE;
            }
        }
    }
}

