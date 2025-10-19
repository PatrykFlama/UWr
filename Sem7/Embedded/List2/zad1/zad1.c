#include <avr/io.h>

volatile unsigned long timer1_millis = 0;
ISR (TIMER1_COMPA_vect)
{
    timer1_millis++;
}

#define LED PB5
#define LED_DDR DDRB
#define LED_PORT PORTB

#define BTN PB4
#define BTN_PIN PINB
#define BTN_PORT PORTB

const int BUFOR_SIZE = 100;
const time_t TIME_OFFSET = 1000;   // milliseconds

struct State
{
    bool btn_state;
    int timestamp;
};


int bufor_begin = 0;
struct State bufor[BUFOR_SIZE];


int main() {
    BTN_PORT |= _BV(BTN);
    LED_DDR |= _BV(LED);

    long start = timer1_millis;
    int last_state = 0;
    int bufor_ptr = 0;

    while(1) {
        // register button change
        const int curr_state = BTN_PIN & _BV(BTN);
        if (curr_state != last_state) {
            last_state = curr_state;

            if (bufor_begin == bufor_ptr) {
                start = timer1_millis;
                bufor[bufor_ptr] = {curr_state, start + TIME_OFFSET};
                bufor_ptr = (bufor_ptr + 1) % BUFOR_SIZE;
            } else {
                bufor[bufor_ptr] = {curr_state, timer1_millis + TIME_OFFSET};
                bufor_ptr = (bufor_ptr + 1) % BUFOR_SIZE;
            }
        }

        // update LED
        if (bufor[bufor_begin].timestamp <= timer1_millis) {
            if (bufor[bufor_begin].btn_state)
                LED_PORT &= ~_BV(LED);
            else
                LED_PORT |= _BV(LED);

            bufor_begin = (bufor_begin + 1) % BUFOR_SIZE;
        }
    }
}

