#include <avr/io.h>
#include <stdio.h>
#include <string.h>
#include <util/delay.h>

#define UART_ECHO
#include "../../customlib/uart.c"
#include "i2c.h"

const uint8_t eeprom_addr = 0x50;

uint8_t eeprom_read(uint16_t addr) {
    uint8_t block = (addr >> 8) & 0x01;
    uint8_t addr_block = eeprom_addr | (block << 1);
    
    // dummy write (set address pointer) 
    i2cStart();
    i2cSend(addr_block << 1);
    i2cSend((uint8_t)(addr & 0xFF));
    
    // data read 
    i2cStart();
    i2cSend((addr_block << 1) | 0x01);

    uint8_t data;
    data = i2cReadNoAck();
    i2cStop();

    return data;
}

void eeprom_write(uint16_t addr, uint8_t data) {
    uint8_t block = (addr >> 8) & 0x01;

    i2cStart();
    i2cSend((eeprom_addr | (block << 1)) << 1);
    i2cSend((uint8_t)(addr & 0xFF));
    i2cSend(data);
    i2cStop();

    _delay_ms(5);
}

int main() {
    char cmd[64];
    uint16_t addr;
    uint16_t value;

    i2cInit();
    uart_init();

    while (1) {
        printf("> ");
        scanf("%63s", cmd);

        if (strcmp(cmd, "read") == 0) {
            scanf("%u", &addr);
            if (addr > 0x1FF) {
                printf("Out of range (0-511)\r\n");
                continue;
            }
            printf("Read addr %u: %u\r\n", addr, eeprom_read(addr));
        } else if (strcmp(cmd, "write") == 0) {
            scanf("%u %u", &addr, &value);
            if (addr > 0x1FF) {
                printf("Out of range (0-511)\r\n");
                continue;
            }
            if (value > 0xFF) {
                printf("Out of range (0-255)\r\n");
                continue;
            }

            eeprom_write(addr, (uint8_t)value);
            printf("Saved %u to addr %u\r\n", value, addr);
        } else {
            printf("unknown command\r\n");
            printf("commands: read <addr>\twrite <addr> <value>\r\n");
        }
    }
}
