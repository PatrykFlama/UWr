#include <avr/io.h>
#include <util/delay.h>

static const uint8_t digits[10] = {
    0b00111111,
    0b00000110,
    0b01011011,
    0b01001111,
    0b01100110,
    0b01101101,
    0b01111101,
    0b00000111,
    0b01111111,
    0b01101111};

#define PIN_LE PB1
#define PIN_OE PB2

void spi_init() {
    // MOSI (PB3), SCK (PB5), LE OE PINS as output
    DDRB |= (1 << PB3) | (1 << PB5) | (1 << PIN_LE) | (1 << PIN_OE);

    // SPI, master, mode 0 (CPOL=0, CPHA=0), zegar F_CPU/128
    SPCR = (1 << SPE) | (1 << MSTR) | (1 << SPR1) | (1 << SPR0);
}

void spi_transfer(uint8_t data) {
    SPDR = data;
    while (!(SPSR & (1 << SPIF)));
}

// send pattern to TLC5916 and pulse LE (latch on falling edge)
void tlc_update(uint8_t pattern) {
    spi_transfer(pattern);

    PORTB |= (1 << PIN_LE);
    _delay_us(1);
    PORTB &= ~(1 << PIN_LE);
}

int main() {
    spi_init();

    uint8_t i = 0;
    while (1) {
        tlc_update(digits[i % 10]);
        i++;
        _delay_ms(1000);
    }
}
