#include <avr/interrupt.h>
#include <avr/io.h>
#include <avr/pgmspace.h>
#include <avr/sleep.h>
#include <stdint.h>
#include <util/delay.h>

#include "dzwiek.c"

#define SAMPLE_RATE 8000U

// BUF (bit14): 1 = Buffered VREF input, 0 = Unbuffered
// GA  (bit13): 1 = 1x (VOUT = VREF * D/4096), 0 = 2x
// SHDN(bit12): 1 = Active (VOUT available), 0 = Shutdown
#define MCP_BUF 0
#define MCP_GA 1
#define MCP_SHDN 1

volatile unsigned int play_pos = 0;

// SPI init: Master, mode 0, fosc/16 (adjust if needed)
void spi_init() {
    // ustaw piny MOSI, SCK i ~SS jako wyjścia
    DDRB |= _BV(DDB3) | _BV(DDB5) | _BV(DDB2);
    // ustaw CS (PB2) w stanie nieaktywnym (wysoki)
    PORTB |= _BV(PORTB2);
    // włącz SPI w trybie master z zegarem 250 kHz
    SPCR = _BV(SPE) | _BV(MSTR) | _BV(SPR1);
}

// transfer jednego bajtu
uint8_t spi_transfer(uint8_t data) {
    // rozpocznij transmisję
    SPDR = data;
    // czekaj na ukończenie transmisji
    while (!(SPSR & _BV(SPIF)));
    // wyczyść flagę przerwania
    SPSR |= _BV(SPIF);
    // zwróć otrzymane dane
    return SPDR;
}

// send 16-bit command to MCP4901 (CS low -> transfer -> CS high)
static void dac_write(const uint16_t cmd) {
    PORTB &= ~_BV(PORTB2);

    spi_transfer((uint8_t)(cmd >> 8));
    spi_transfer((uint8_t)(cmd & 0xFF));

    PORTB |= _BV(PORTB2);
}

static void timer1_init(uint32_t sample_rate) {
    // ticks = F_CPU / prescaler / sample_rate - 1
    const uint32_t ticks = (F_CPU / 8U) / sample_rate;

    TCCR1A = 0;
    TCCR1B = _BV(WGM12) | _BV(CS11);  // CTC, prescaler 8
    OCR1A = (uint16_t)(ticks - 1);
    TIMSK1 = _BV(OCIE1A);  // enable compare A interrupt
}

// Timer1 compare interrupt - output next sample
ISR(TIMER1_COMPA_vect) {
    uint8_t s = pgm_read_byte(&dzwiek_raw[play_pos]);

    const uint16_t control = ((uint16_t)MCP_BUF << 14) |
                             ((uint16_t)MCP_GA << 13) |
                             ((uint16_t)MCP_SHDN << 12);
    const uint16_t cmd = (uint16_t)(control | ((uint16_t)s << 4));

    dac_write(cmd);

    play_pos++;
    if (play_pos >= dzwiek_raw_len) {
        play_pos = 0;
        _delay_ms(500);
    }
}

int main() {
    spi_init();
    timer1_init(SAMPLE_RATE);

    set_sleep_mode(SLEEP_MODE_IDLE);
    sei();

    while (1) {
        sleep_mode();
    }
}
