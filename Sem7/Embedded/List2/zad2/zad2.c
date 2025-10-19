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

#define dprintf if (1) printf

#define UNIT_MS 200 // base unit in ms (dot length)
#define BUFFER_SIZE 16

// 3 bits for length, 5 bits for sequence (dot=0, dash=1)
static const uint8_t morse_letters[26] = {
    0b01000001, /* A  .-    */
    0b10001000, /* B  -...  */
    0b10001010, /* C  -.-.  */
    0b01100100, /* D  -..   */
    0b00100000, /* E  .     */
    0b10000010, /* F  ..-.  */
    0b01100110, /* G  --.   */
    0b10000000, /* H  ....  */
    0b01000000, /* I  ..    */
    0b10000111, /* J  .---  */
    0b01100101, /* K  -.-   */
    0b10000100, /* L  .-..  */
    0b01000011, /* M  --    */
    0b01000010, /* N  -.    */
    0b01100111, /* O  ---   */
    0b10000110, /* P  .--.  */
    0b10001101, /* Q  --.-  */
    0b01100010, /* R  .-.   */
    0b01100000, /* S  ...   */
    0b00100001, /* T  -     */
    0b01100001, /* U  ..-   */
    0b10000001, /* V  ...-  */
    0b01100011, /* W  .--   */
    0b10001001, /* X  -..-  */
    0b10001011, /* Y  -.--  */
    0b10001100  /* Z  --..  */
};

static const uint8_t morse_digits[10] = {
    0b10101111, /* 1 .---- */
    0b10100111, /* 2 ..--- */
    0b10100011, /* 3 ...-- */
    0b10100001, /* 4 ....- */
    0b10100000, /* 5 ..... */
    0b10110000, /* 6 -.... */
    0b10111000, /* 7 --... */
    0b10111100, /* 8 ---.. */
    0b10111110, /* 9 ----. */
    0b10111111  /* 0 ----- */
};


static char decode_morse(int press_pattern, int len) {
    for (int i = len; i < 5; i++) {
        press_pattern <<= 1;
    }
    const int pattern = (len << 5) | press_pattern;
    dprintf("Decoding pattern: ");
    for (int i = 7; i >= 0; --i) dprintf("%c", (press_pattern & (1 << i)) ? '1' : '0');
    dprintf(" %d ", len);
    for (int i = 7; i >= 0; --i) dprintf("%c", (pattern & (1 << i)) ? '1' : '0');
    dprintf("\r\n");
    // try letters
    for (int i = 0; i < 26; i++) {
        if (morse_letters[i] == pattern) {
            return 'A' + i;
        }
    }

    // try digits
    for (int i = 0; i < 10; i++) {
        if (morse_digits[i] == pattern) {
            return '0' + i;
        }
    }

    return '?';
}


int main(void) {
    BTN_PORT |= _BV(BTN); // pull-up
    LED_DDR |= _BV(LED);  // LED output

    uart_init();
    timer_init_ms();

    const unsigned dot_time = UNIT_MS;
    const unsigned dash_time = 3 * UNIT_MS;
    const unsigned inter_char_time = 3 * UNIT_MS;
    const unsigned inter_word_time = 7 * UNIT_MS;

    unsigned long last_event_time = millis();
    int last_btn = (BTN_PIN & _BV(BTN)) ? 1 : 0; // 1 = not pressed (pull-up)

    int buffer = 0;
    int buffer_len = 0;
    int word_gapped = 0;

    printf("Morse decoder started\r\n");

    while (1) {
        const int btn = (BTN_PIN & _BV(BTN)) ? 1 : 0;
        const unsigned long now = millis();

        // button pressed
        if (!btn && last_btn) {
            LED_PORT |= _BV(LED);
            last_event_time = now;
            word_gapped = 0;
        }

        // button released
        if (btn && !last_btn) {
            LED_PORT &= ~_BV(LED);
            const unsigned long press_dur = now - last_event_time;
            
            if (press_dur <= dot_time) {
                buffer = (buffer << 1) | 0b00000001;
                buffer_len++;
                dprintf(".");
            } else {
                buffer = buffer << 1;
                buffer_len++;
                dprintf("-");
            }

            last_event_time = now;
        }

        // if button not pressed, check gaps to determine separation
        if (btn) {
            const unsigned long gap_dur = now - last_event_time;
            if (buffer > 0 && gap_dur >= inter_char_time) {
                // end of character
                const char ch = decode_morse(buffer, buffer_len);
                printf("%c ", ch);
                buffer = 0;
                buffer_len = 0;
                last_event_time = now;
            } else if (word_gapped && buffer == 0 && gap_dur >= inter_word_time) {
                // word gap
                printf("\r\n");
                last_event_time = now;
                word_gapped = 1;
            }
        }

        last_btn = btn;
    }
}
