#include <avr/io.h>
#include <util/delay.h>

#include "../../customlib/millis.c"
#include "../../customlib/uart.c"


#define LED PB5
#define LED_DDR DDRB
#define LED_PORT PORTB

#define BTN PB4
#define BTN_PIN PINB
#define BTN_PORT PORTB

#ifndef DEBUG
#define dprintf(...) ((void)0)
#else
#define dprintf(...) printf(__VA_ARGS__)
#endif

#define UNIT_MS 200 // base unit in ms (dot length)
#define BUFFER_SIZE 16

static const char trie[] = {
    0, 
    0,
    'E', 'T', 
    'I', 'A', 'N', 'M', 
    'S', 'U', 'R', 'W', 'D', 'K', 'G', 'O', 
    0, 'H', 'V', 'F', 0, 'L', 'P', 'J', 'B', 'X', 'C', 'Y', 'Z', 'Q', 0, 0, 0, 0
};

/*
A  .-   
B  -... 
C  -.-. 
D  -..  
E  .    
F  ..-. 
G  --.  
H  .... 
I  ..   
J  .--- 
K  -.-  
L  .-.. 
M  --   
N  -.   
O  ---  
P  .--. 
Q  --.- 
R  .-.  
S  ...  
T  -    
U  ..-  
V  ...- 
W  .--  
X  -..- 
Y  -.-- 
Z  --.. 
1 .----
2 ..---
3 ...--
4 ....-
5 .....
6 -....
7 --...
8 ---..
9 ----.
0 -----
*/

static char decode_morse(int press_pattern, int len) {
    int pos = 1;
    for (int i = 0; i < len; i++) {
        if (press_pattern & (1 << (len - 1 - i))) {
            // dash
            pos = (pos << 1) | 1;
        } else {
            // dot
            pos = (pos << 1) | 0;
        }
    }
    return trie[pos];
}


int main() {
    BTN_PORT |= _BV(BTN); // pull-up
    LED_DDR |= _BV(LED);  // LED output

    uart_init();
    timer_init_ms();

    const unsigned dot_time = UNIT_MS;
    const unsigned dash_time = 3 * UNIT_MS;
    const unsigned inter_char_time = 3 * UNIT_MS;
    const unsigned inter_word_time = 7 * UNIT_MS;

    unsigned int last_event_time = millis();
    int last_btn = (BTN_PIN & _BV(BTN)) ? 1 : 0; // 1 = not pressed (pull-up)

    int buffer = 0;
    int buffer_len = 0;
    int word_gapped = 1;

    printf("Morse decoder started\r\n");

    while (1) {
        const int btn = (BTN_PIN & _BV(BTN)) ? 1 : 0;
        const unsigned int now = millis();

        // button pressed
        if (!btn && last_btn) {
            _delay_ms(10);
            if ((BTN_PIN & _BV(BTN)) ? 1 : 0 != btn) continue;

            LED_PORT |= _BV(LED);
            last_event_time = now;
            word_gapped = 0;
            _delay_ms(10);
        }

        // button released
        if (btn && !last_btn) {
            LED_PORT &= ~_BV(LED);
            const unsigned int press_dur = now - last_event_time;
            
            if (press_dur <= dot_time) {
                buffer = buffer << 1;
                buffer_len++;
                dprintf(".");
            } else {
                buffer = (buffer << 1) | 1;
                buffer_len++;
                dprintf("-");
            }

            last_event_time = now;
            _delay_ms(10);
        }

        // if button not pressed, check gaps to determine separation
        if (btn) {
            const unsigned int gap_dur = now - last_event_time;
            if (buffer_len > 0 && gap_dur >= inter_char_time) {
                // end of character
                const char ch = decode_morse(buffer, buffer_len);
                printf("%c ", ch);
                buffer = 0;
                buffer_len = 0;
                last_event_time = now;
                word_gapped = 0;
            } else if (!word_gapped && buffer == 0 && gap_dur >= inter_word_time) {
                // word gap
                printf("\r\n");
                last_event_time = now;
                word_gapped = 1;
            }
        }

        last_btn = btn;
    }
}
