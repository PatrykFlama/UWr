#include <avr/interrupt.h>
#include <avr/io.h>
#include <inttypes.h>

volatile uint8_t slave_data = 0;

void spi_slave_init() {
    // MISO as input, rest as output
    DDRB |= _BV(DDB4);
    DDRB &= ~(_BV(DDB2) | _BV(DDB3) | _BV(DDB5));
    // pull-up on SS
    PORTB |= _BV(PB2);
    // enable SPI in slave mode with interrupt
    SPCR = _BV(SPE) | _BV(SPIE);
}

ISR(SPI_STC_vect) {
    uint8_t received = SPDR;
    // set LED based on received data
    if (received) {
        PORTB |= _BV(PB5);
    } else {
        PORTB &= ~_BV(PB5);
    }
    // prepare data to send (button state)
    slave_data = (PIND & _BV(PD2)) ? 0 : 1;
    SPDR = slave_data;
}

int main() {
    // LED (PB5) as output
    DDRB |= _BV(DDB5);
    // button (PD2) as input with pull-up
    DDRD &= ~_BV(DDD2);
    PORTD |= _BV(PD2);

    spi_slave_init();

    // prepare first value to send
    slave_data = (PIND & _BV(PD2)) ? 0 : 1;
    SPDR = slave_data;

    set_sleep_mode(SLEEP_MODE_IDLE);

    sei();

    while (1) {
        sleep_mode();
    }
}
