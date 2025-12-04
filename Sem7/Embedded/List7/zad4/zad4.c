#include <avr/interrupt.h>
#include <avr/io.h>
#include <inttypes.h>
#include <stdio.h>
#include <util/delay.h>

#include "../../customlib/uart.c"

// Bit-banged (software) SPI definitions - master side
#define SWSPI_PORT PORTD
#define SWSPI_DDR DDRD
#define SWSPI_PIN PIND

#define SWSPI_SS PD4
#define SWSPI_MOSI PD5
#define SWSPI_MISO PD6
#define SWSPI_SCK PD7

// Hardware SPI (slave) pin identifiers
#define HWSPI_SS PB2
#define HWSPI_MOSI PB3
#define HWSPI_MISO PB4
#define HWSPI_SCK PB5

volatile uint8_t slave_rx = 0;
volatile uint8_t slave_has_new = 0;

// Initialize software SPI pins (master)
static void swspi_setup() {
    // SS, MOSI, SCK as outputs; MISO as input
    SWSPI_DDR |= (1 << SWSPI_SS) | (1 << SWSPI_MOSI) | (1 << SWSPI_SCK);
    SWSPI_DDR &= ~(1 << SWSPI_MISO);
    // Pull-up on MISO so slave can drive it
    SWSPI_PORT |= (1 << SWSPI_MISO);

    // Idle: SS high (inactive), SCK low (SPI mode 0)
    SWSPI_PORT |= (1 << SWSPI_SS);
    SWSPI_PORT &= ~(1 << SWSPI_SCK);
}

// Transfer one byte over software SPI (MSB first)
static uint8_t swspi_xfer(uint8_t out) {
    uint8_t in = 0;

    // Assert slave select
    SWSPI_PORT &= ~(1 << SWSPI_SS);
    _delay_us(1);

    for (int8_t bit = 7; bit >= 0; --bit) {
        // Drive MOSI according to current bit
        if (out & (1u << bit)) {
            SWSPI_PORT |= (1 << SWSPI_MOSI);
        } else {
            SWSPI_PORT &= ~(1 << SWSPI_MOSI);
        }

        // Clock high: slave samples MOSI
        _delay_us(4);
        SWSPI_PORT |= (1 << SWSPI_SCK);

        // Read MISO
        in = (in << 1) | ((SWSPI_PIN & (1 << SWSPI_MISO)) ? 1u : 0u);

        // Clock low: slave may change MISO
        _delay_us(4);
        SWSPI_PORT &= ~(1 << SWSPI_SCK);
    }

    // Deassert slave select
    SWSPI_PORT |= (1 << SWSPI_SS);

    return in;
}

// Configure hardware SPI in slave mode with interrupts enabled
static void hwspi_slave_setup() {
    // MISO output, other SPI pins inputs
    DDRB |= (1 << HWSPI_MISO);
    DDRB &= ~((1 << HWSPI_MOSI) | (1 << HWSPI_SCK) | (1 << HWSPI_SS));

    // Keep SS pulled-up when idle
    PORTB |= (1 << HWSPI_SS);

    // Enable SPI and its interrupt (slave mode)
    SPCR = (1 << SPE) | (1 << SPIE);
}

// SPI Transfer Complete interrupt (hardware slave)
ISR(SPI_STC_vect) {
    slave_rx = SPDR;  // capture received byte
    slave_has_new = 1;
    SPDR = (uint8_t)(slave_rx + 1);  // prepare response for next transfer
}

int main() {
    uart_init();

    // Ensure slave side is ready before master attempts to talk
    hwspi_slave_setup();
    swspi_setup();

    sei();

    uint8_t counter = 1;

    for (;;) {
        printf("Master sending: %u\r\n", counter);

        uint8_t resp = swspi_xfer(counter);

        if (slave_has_new) {
            slave_has_new = 0;
            printf("Slave received: %u\r\n", slave_rx);
            printf("Master received back: %u\r\n", resp);
            printf("---\r\n");
        } else {
            printf("Slave did not respond\r\n");
        }

        ++counter;
        _delay_ms(1000);
    }
}
