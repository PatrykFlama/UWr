#include <avr/io.h>
#include <avr/pgmspace.h>
#include <stdint.h>
#include <util/delay.h>

#define BUZZ PB1
#define BUZZ_DDR DDRB
#define BUZZ_PORT PORTB

static const int silent = 0;
static const int c = 261;
static const int d = 294;
static const int e = 329;
static const int f = 349;
static const int g = 391;
static const int gS = 415;
static const int a = 440;
static const int aS = 455;
static const int b = 466;
static const int cH = 523;
static const int cSH = 554;
static const int dH = 587;
static const int eH = 659;
static const int fH = 698;
static const int fSH = 740;
static const int gH = 784;
static const int gSH = 830;
static const int aH = 880;

typedef struct {
    uint16_t freq;  // Hz, 0 = pauza
    uint16_t dur;   // ms
} Note;

// Melodia w PROGMEM: 8-nutowy motyw powtórzony 8 razy -> 8*8*0.5s = 32s
const Note melody[] PROGMEM = {
    // section 1
    {a, 500},
    {a, 500},
    {a, 500},
    {f, 350},
    {cH, 150},
    {a, 500},
    {f, 350},
    {cH, 150},
    {a, 650},

    {silent, 500},

    {eH, 500},
    {eH, 500},
    {eH, 500},
    {fH, 350},
    {cH, 150},
    {gS, 500},
    {f, 350},
    {cH, 150},
    {a, 650},

    {silent, 500},

    // section 2
    {aH, 500},
    {a, 300},
    {a, 150},
    {aH, 500},
    {gSH, 325},
    {gH, 175},
    {fSH, 125},
    {fH, 125},
    {fSH, 250},

    {silent, 325},

    {aS, 250},
    {dSH, 500},
    {dH, 325},
    {cSH, 175},
    {cH, 125},
    {b, 125},
    {cH, 250},

    {silent, 350},9

    // variant 1
    {f, 250},
    {gS, 500},
    {f, 350},
    {a, 125},
    {cH, 500},
    {a, 375},
    {cH, 125},
    {eH, 650},

    {silent, 500},

    // section 2
    {aH, 500},
    {a, 300},
    {a, 150},
    {aH, 500},
    {gSH, 325},
    {gH, 175},
    {fSH, 125},
    {fH, 125},
    {fSH, 250},

    {silent, 325},

    {aS, 250},
    {dSH, 500},
    {dH, 325},
    {cSH, 175},
    {cH, 125},
    {b, 125},
    {cH, 250},

    {silent, 350},

    // variant 2
    {f, 250},
    {gS, 500},
    {f, 375},
    {cH, 125},
    {a, 500},
    {f, 375},
    {cH, 125},
    {a, 650},

    {silent, 650},

};

#define MELODY_LEN (sizeof(melody) / sizeof(melody[0]))

static void play_tone(uint32_t step_us, uint16_t delay_ms) {
    if (step_us == 0 || delay_ms == 0) return;

    // number of half-period pairs (high+low) to generate
    uint32_t loops = (1000UL * (uint32_t)delay_ms) / (step_us) / 2;
    if (loops == 0) loops = 1;

    for (uint32_t i = 0; i < loops; i++) {
        BUZZ_PORT |= _BV(BUZZ);
        for (uint32_t t = 0; t < step_us; t++)
            _delay_us(1);

        BUZZ_PORT &= ~_BV(BUZZ);
        for (uint32_t t = 0; t < step_us; t++)
            _delay_us(1);
    }
}

int main() {
    BUZZ_DDR |= _BV(BUZZ);  // set pin as outpput

    while (1) {
        for (uint16_t i = 0; i < MELODY_LEN; i++) {
            uint16_t freq = pgm_read_word(&melody[i].freq);
            uint16_t dur = pgm_read_word(&melody[i].dur);

            if (freq == 0) {
                for (uint16_t m = 0; m < dur; m++)
                    _delay_ms(1);
            } else {
                // calculate half-period in microseconds
                uint32_t step = 1000000UL / ((uint32_t)freq * 2UL);
                uint16_t play_ms = (uint32_t)dur * 9 / 10;
                if (step < 1) step = 1;
                if (step > 60000) step = 60000;
                play_tone(step, play_ms);

                uint16_t rem = dur - play_ms;
                for (uint16_t m = 0; m < rem; m++)
                    _delay_ms(1);
            }
        }

        _delay_ms(3000);
    }
}
