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

int buffer_begin = 0; // index of oldest event
int buffer_end = 0;   // index one past last event (empty when equal to begin)
struct State buffer[BUFOR_SIZE];


int main() {
    // enable pull-up on button pin, set LED as output
    BTN_PORT |= _BV(BTN);
    LED_DDR |= _BV(LED);

    // initialize timer
    timer_init_ms();

    uint8_t last_state = (BTN_PIN & _BV(BTN)) ? 1 : 0;

    while (1) {
        // register button change
        const uint8_t curr_state = (BTN_PIN & _BV(BTN)) ? 1 : 0;
        if (curr_state != last_state) {
            last_state = curr_state;

            unsigned long play_time = timer_millis + TIME_OFFSET;

            int next_end = (buffer_end + 1) % BUFOR_SIZE;
            // if buffer full, drop oldest (advance begin)
            if (next_end == buffer_begin) {
                buffer_begin = (buffer_begin + 1) % BUFOR_SIZE;
            }

            buffer[buffer_end].btn_state = curr_state;
            buffer[buffer_end].timestamp = play_time;
            buffer_end = next_end;
        }

        // replay events when their timestamp has passed
        if (buffer_begin != buffer_end) {
            if (timer_millis >= buffer[buffer_begin].timestamp) {
                if (buffer[buffer_begin].btn_state)
                    LED_PORT &= ~_BV(LED);
                else
                    LED_PORT |= _BV(LED);

                buffer_begin = (buffer_begin + 1) % BUFOR_SIZE;
            }
        }
    }
}

