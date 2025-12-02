#include <avr/io.h>
#include <stdio.h>
#include <string.h>
#include <util/delay.h>

#define UART_ECHO
#include "../../customlib/uart.c"
#include "i2c.h"

#define EEPROM_ADDR 0x50  // (1010 000x)

uint8_t eeprom_read(uint16_t addr) {
    uint8_t data;

    // Extract bit A8 (select memory block 0 or 1)
    // 24C04 has 512 bytes = 2 blocks of 256 bytes
    // addr >> 8 shifts right by 8 bits to obtain bit 8
    // & 0x01 masks all bits except the least significant one
    uint8_t block = (addr >> 8) & 0x01;

    // Build the I2C device address including the block bit
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

    // === PHASE 1: Dummy Write - set EEPROM internal address pointer ===

    i2cStart();

    // Send device address with W=0 (write)
    // device_addr << 1 creates an 8-bit byte with R/W=0 in LSB
    // Example: 0x50 << 1 = 0xA0 = 0b10100000
    i2cSend(device_addr << 1);

    // Send the byte address within the selected block (A7-A0)
    // addr & 0xFF masks bits 8-15, leaving only A7-A0
    i2cSend((uint8_t)(addr & 0xFF));

    // === PHASE 2: Actual data read ===

    i2cStart();

    // Send device address with R=1 (read)
    // | 0x01 sets the least significant bit (R/W) to 1
    // Example: (0x50 << 1) | 0x01 = 0xA1 = 0b10100001
    i2cSend((device_addr << 1) | 0x01);

    // Read a data byte from the EEPROM
    data = i2cReadNoAck();
    i2cStop();
    return data;
}

void eeprom_write(uint16_t addr, uint8_t data) {
    // Extract bit A8 (select memory block)
    uint8_t block = (addr >> 8) & 0x01;

    // Build the I2C device address including the block bit
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

    // Send the byte address within the block (A7-A0)
    i2cSend((uint8_t)(addr & 0xFF));

    // Send the data byte to be written
    i2cSend(data);

    i2cStop();

    // Wait for the internal write cycle to finish (tWR)
    // According to datasheet: tWR max = 5ms, we use 10ms for safety
    _delay_ms(10);
}

int main() {
    char cmd[10];
    uint16_t addr;
    uint16_t value;

    i2cInit();
    uart_init();

    while (1) {
        printf("> ");
        scanf("%s", cmd);

        if (strcmp(cmd, "read") == 0) {
            scanf("%u", &addr);
            if (addr > 0x1FF) {
                printf("Error: Address out of range (0-511)\r\n");
                continue;
            }
            printf("Read addr %u: %u\r\n", addr, eeprom_read(addr));
        } else if (strcmp(cmd, "write") == 0) {
            scanf("%u %u", &addr, &value);
            if (addr > 0x1FF) {
                printf("Error: Address out of range (0-511)\r\n");
                continue;
            }
            if (value > 0xFF) {
                printf("Error: Value out of range (0-255)\r\n");
                continue;
            }

            eeprom_write(addr, (uint8_t)value);
            printf("Wrote value %u to addr %u\r\n", value, addr);
        } else {
            printf("unknown command\r\n");
            printf("commands: read <addr>\twrite <addr> <value>\r\n");
        }
    }
}
