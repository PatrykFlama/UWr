#include <avr/interrupt.h>
#include <avr/io.h>
#include <inttypes.h>
#include <avr/sleep.h>
#include <util/delay.h>


// inicjalizacja SPI
void spi_init()
{
    // ustaw piny MOSI, SCK i ~SS jako wyjścia
    DDRB &= ~(_BV(DDB3) | _BV(DDD3) | _BV(DDB2));

    // miso jako wejścir
    DDRB |= _BV(DDB4);

    // spi w trybie slave
    SPCR = _BV(SPE);
}

// transfer jednego bajtu
uint8_t spi_transfer(uint8_t data)
{
    // rozpocznij transmisję
    SPDR = data;
    // czekaj na ukończenie transmisji
    while (!(SPSR & _BV(SPIF)));
    // wyczyść flagę przerwania
    SPSR |= _BV(SPIF);
    // zwróć otrzymane dane
    return SPDR;
}

int main() {
    // LED (PD3) as output
    DDRD |= _BV(DDD3);
    // button (PD2) as input with pull-up
    DDRD &= ~_BV(DDD2);
    PORTD |= _BV(PD2);

    spi_init();

    while (1) {
        // read button state
        uint8_t btn_state = (PIND & _BV(PD2)) ? 0 : 1;
        // send button state via SPI and receive data
        uint8_t received = spi_transfer(btn_state);
        // set LED according to received data
        if (received) {
            PORTD |= _BV(PD3);
        } else {
            PORTD &= ~_BV(PD3);
        }
    }
}
