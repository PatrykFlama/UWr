#include <avr/io.h>
#include <inttypes.h>
#include <stdio.h>
#include <util/delay.h>

#include "i2c.h"

#define UART_ECHO
#include "../../customlib/uart.c"

const uint8_t eeprom_addr = 0xa0;  // device base address (A2..A0 = 0)

int eeprom_write_byte(uint16_t addr, uint8_t value) {
    i2cStart();
    if ((TWSR & 0xF8) != 0x08) {
        i2cStop();
        return -1;
    }
    i2cSend(eeprom_addr | ((addr & 0x100) >> 7));
    if ((TWSR & 0xF8) != 0x18) {
        i2cStop();
        return -1;
    }
    i2cSend(addr & 0xFF);
    if ((TWSR & 0xF8) != 0x28) {
        i2cStop();
        return -1;
    }
    i2cSend(value);
    if ((TWSR & 0xF8) != 0x28) {
        i2cStop();
        return -1;
    }
    i2cStop();
    _delay_ms(10);  // wait for EEPROM write cycle
    
    return 0;
}

int eeprom_read_byte(uint16_t addr, uint8_t* out) {
    i2cStart();
    if ((TWSR & 0xF8) != 0x08) {
        i2cStop();
        return -1;
    }
    i2cSend(eeprom_addr | ((addr & 0x100) >> 7));
    if ((TWSR & 0xF8) != 0x18) {
        i2cStop();
        return -1;
    }
    i2cSend(addr & 0xFF);
    if ((TWSR & 0xF8) != 0x28) {
        i2cStop();
        return -1;
    }
    i2cStart();
    if ((TWSR & 0xF8) != 0x10) {
        i2cStop();
        return -1;
    }
    i2cSend(eeprom_addr | 0x1 | ((addr & 0x100) >> 7));
    if ((TWSR & 0xF8) != 0x40) {
        i2cStop();
        return -1;
    }
    uint8_t v = i2cReadNoAck();
    if ((TWSR & 0xF8) != 0x58) {
        i2cStop();
        return -1;
    }
    i2cStop();
    *out = v;
    return 0;
}

int main() {
    uart_init();

    i2cInit();

    char line[64];
    printf("EEPROM I2C console ready\r\n");

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
            if (eeprom_read_byte((uint16_t)addr, &v) == 0) {
                printf("0x%03x: 0x%02x\r\n", addr, v);
            } else {
                printf("read failed at 0x%03x\r\n", addr);
            }
        } else if (sscanf(line, "write %i %i", &addr, &value) == 2) {
            if (addr > 0x1FF) {
                printf("addr out of range (0..0x1FF)\r\n");
                continue;
            }
            if (value > 0xFF) {
                printf("value out of range (0..0xFF)\r\n");
                continue;
            }
            if (eeprom_write_byte((uint16_t)addr, (uint8_t)value) == 0) {
                printf("wrote 0x%02x to 0x%03x\r\n", value, addr);
            } else {
                printf("write failed at 0x%03x\r\n", addr);
            }
        } else if (idx == 0) {
            // empty line, ignore
        } else {
            printf("unknown command: '%s'\r\n", line);
            printf("commands: read addr\twrite addr value\r\n");
        }
    }
}
