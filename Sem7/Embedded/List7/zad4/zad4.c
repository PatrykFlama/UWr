#include <avr/interrupt.h>
#include <avr/io.h>
#include <inttypes.h>
#include <stdio.h>
#include <util/delay.h>

#include "../../customlib/uart.c"

// --- Software SPI pins configuration (Master) ---
// Using port D
#define SW_SPI_PORT PORTD
#define SW_SPI_DDR DDRD
#define SW_SPI_PIN PIND

#define SW_SS PD4
#define SW_MOSI PD5
#define SW_MISO PD6
#define SW_SCK PD7

// --- Hardware SPI pins configuration (Slave) ---
// Using port B
#define HW_SS PB2
#define HW_MOSI PB3
#define HW_MISO PB4
#define HW_SCK PB5

volatile uint8_t slave_received_data = 0;
volatile uint8_t slave_new_data_flag = 0;

void spi_init() {
    // Configure directions: SS, MOSI, SCK as outputs
    SW_SPI_DDR |= (1 << SW_SS) | (1 << SW_MOSI) | (1 << SW_SCK);
    // MISO as input
    SW_SPI_DDR &= ~(1 << SW_MISO);
    // Enable pull-up on MISO
    SW_SPI_PORT |= (1 << SW_MISO);

    // Initial state: SS high (inactive), SCK low (Mode 0)
    SW_SPI_PORT |= (1 << SW_SS);
    SW_SPI_PORT &= ~(1 << SW_SCK);
}

uint8_t spi_transfer(uint8_t data) {
    uint8_t received = 0;

    // Activate Slave (SS Low)
    SW_SPI_PORT &= ~(1 << SW_SS);
    _delay_us(1);  // Short delay for stabilization

    // Loop for 8 bits (MSB first)
    for (uint8_t i = 0; i < 8; i++) {
        // 1. Set MOSI according to the data bit (MSB)
        if (data & 0x80) {
            SW_SPI_PORT |= (1 << SW_MOSI);
        } else {
            SW_SPI_PORT &= ~(1 << SW_MOSI);
        }

        // Shift data to the next bit
        data <<= 1;

        // 2. Rising edge SCK (Clock High) - Slave reads MOSI
        _delay_us(5);  // Delay for clock half-period
        SW_SPI_PORT |= (1 << SW_SCK);

        // 3. Read MISO (MSB)
        received <<= 1;
        if (SW_SPI_PIN & (1 << SW_MISO)) {
            received |= 1;
        }

        // 4. Falling edge SCK (Clock Low) - Slave drives MISO
        _delay_us(5);
        SW_SPI_PORT &= ~(1 << SW_SCK);
    }

    // Deactivate Slave (SS High)
    SW_SPI_PORT |= (1 << SW_SS);

    return received;
}

void spi_slave_init() {
    // Configure directions for Slave: MISO output, others input
    DDRB |= (1 << HW_MISO);                                    // MISO Output
    DDRB &= ~((1 << HW_MOSI) | (1 << HW_SCK) | (1 << HW_SS));  // MOSI, SCK, SS Input

    // Enable pull-up on SS
    PORTB |= (1 << HW_SS);

    // Configure SPCR register:
    // SPE (SPI Enable) - enable SPI
    // SPIE (SPI Interrupt Enable) - enable interrupts
    // Bit MSTR = 0 (Slave mode)
    SPCR = (1 << SPE) | (1 << SPIE);
}

// Hardware SPI Slave interrupt handler
// Called when the Slave receives a full byte
ISR(SPI_STC_vect) {
    slave_received_data = SPDR;  // Odczytaj odebrane dane
    slave_new_data_flag = 1;     // Ustaw flagę
    // Załaduj dane do wysłania w następnej transakcji
    SPDR = slave_received_data + 1;
}

int main() {
    uart_init();

    spi_slave_init();  // Initialize Slave first
    spi_init();        // Then initialize Master

    sei();

    uint8_t counter = 1;

    while (1) {
        printf("Master sending: %u\r\n", counter);

        // Master wysyła dane i odbiera odpowiedź (z poprzedniej transakcji)
        uint8_t master_received = spi_transfer(counter);

        // Check if the Slave received data (interrupt should have fired)
        if (slave_new_data_flag) {
            slave_new_data_flag = 0;
            printf("Slave received: %u\r\n", slave_received_data);
            printf("Master received back: %u\r\n", master_received);
            printf("---\r\n");
        } else {
            printf("Error: Slave did not respond!\r\n");
        }

        counter++;
        _delay_ms(1000);
    }
}
