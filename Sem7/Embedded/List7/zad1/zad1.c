#include <avr/io.h>
#include <inttypes.h>
#include <stdio.h>
#include <util/delay.h>

#include "i2c.h"

#define UART_ECHO
#include "../../customlib/uart.c"

const uint8_t eeprom_addr = 0xa0;  // device base address (A2..A0 = 0)

#define i2cCheck(code)             \
    if ((TWSR & 0xf8) != (code)) { \
        i2cStop();                 \
        return -1;                 \
    }

uint8_t eeprom_read(uint16_t addr) {
    uint8_t data;

    // Extract bit A8 (select memory block 0 or 1)
    // 24C04 has 512 bytes = 2 blocks of 256 bytes
    // addr >> 8 shifts right by 8 to get bit 8
    // & 0x01 masks all bits except the least significant one
    uint8_t block = (addr >> 8) & 0x01;

    // Construct I2C device address with the block bit
    // Format: 1010 A2 A1 A8 (without R/W bit)
    // EEPROM_ADDR = 0x50 = 0b01010000 (1010 00x0)
    // block << 1 moves the block bit into the A8 position
    uint8_t device_addr = EEPROM_ADDR | (block << 1);

    // START
    // DEVICE_ADDR(W)
    // WORD_ADDR
    // START
    // DEVICE_ADDR(R)
    // DATA
    // STOP

    // === PHASE 1: Dummy Write - set the internal address pointer in EEPROM ===

    i2cStart();

    // Send device address with W=0 (write)
    // device_addr << 1 forms the 8-bit byte with R/W=0 at the LSB
    // Example: 0x50 << 1 = 0xA0 = 0b10100000
    i2cSend(device_addr << 1);

    // Send the byte address within the chosen block (lower 8 bits A7-A0)
    // addr & 0xFF masks off bits 8-15, leaving A7-A0 only
    i2cSend((uint8_t)(addr & 0xFF));

    // === PHASE 2: Actual data read ===

    i2cStart();

    // Send device address with R=1 (read)
    // | 0x01 sets the least significant bit (R/W) to 1
    // Example: (0x50 << 1) | 0x01 = 0xA1 = 0b10100001
    i2cSend((device_addr << 1) | 0x01);

    // Odczytaj bajt danych z EEPROM
    data = i2cReadNoAck();
    i2cStop();
    return data;
}

void eeprom_write(uint16_t addr, uint8_t data) {
    // Extract bit A8 (select memory block)
    uint8_t block = (addr >> 8) & 0x01;

    // Construct I2C device address with the block bit
    uint8_t device_addr = EEPROM_ADDR | (block << 1);

    // START
    // DEVICE_ADDR(W)
    // WORD_ADDR
    // DATA
    // STOP
    // [wait tWR=10ms]

    i2cStart();

    // Send device address with W=0 (write)
    i2cSend(device_addr << 1);

    // Send byte address within the block (A7-A0)
    i2cSend((uint8_t)(addr & 0xFF));

    // Send data byte to write
    i2cSend(data);

    i2cStop();

    // Wait for internal write cycle to finish (tWR)
    // According to the datasheet: tWR max = 5ms, we use 10ms for safety
    _delay_ms(10);
}

int main() {
    uart_init();

    i2cInit();

    char line[64];

    while (1) {
        // prompt
        printf("> ");

        // read line from UART
        uint8_t idx = 0;
        int c;
        // read until newline
        while (1) {
            c = getchar();
            if (c == '\r') break;
            if (c == '\n') break;
            if (c == EOF) break;
            if (idx < sizeof(line) - 1) line[idx++] = (char)c;
        }
        line[idx] = '\0';

        // parse commands
        unsigned int addr = 0, value = 0;
        if (sscanf(line, "read %i", &addr) == 1) {
            if (addr > 0x1FF) {
                printf("addr out of range (0..0x1FF)\r\n");
                continue;
            }
            uint8_t v;
            v = eeprom_read_byte((uint16_t)addr);
            printf("0x%03x: 0x%02x\r\n", addr, v);
        } else if (sscanf(line, "write %i %i", &addr, &value) == 2) {
            if (addr > 0x1FF) {
                printf("addr out of range (0..0x1FF)\r\n");
                continue;
            }
            if (value > 0xFF) {
                printf("value out of range (0..0xFF)\r\n");
                continue;
            }

            eeprom_write_byte((uint16_t)addr, (uint8_t)value);
            printf("wrote 0x%02x to 0x%03x\r\n", value, addr);
        } else if (idx == 0) {
            // empty line, ignore
        } else {
            printf("unknown command: '%s'\r\n", line);
            printf("commands: read addr\twrite addr value\r\n");
        }
    }
}
