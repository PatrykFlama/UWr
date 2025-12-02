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

    uint8_t block = (addr >> 8) & 0x01;

    uint8_t device_addr = EEPROM_ADDR | (block << 1);

    // Dummy Write (set EEPROM internal address pointer) 

    i2cStart();

    i2cSend(device_addr << 1);

    i2cSend((uint8_t)(addr & 0xFF));

    // data read 

    i2cStart();

    i2cSend((device_addr << 1) | 0x01);

    data = i2cReadNoAck();
    i2cStop();
    return data;
}

void eeprom_write(uint16_t addr, uint8_t data) {
    uint8_t block = (addr >> 8) & 0x01;

    uint8_t device_addr = EEPROM_ADDR | (block << 1);

    i2cStart();

    i2cSend(device_addr << 1);

    i2cSend((uint8_t)(addr & 0xFF));

    i2cSend(data);

    i2cStop();

    _delay_ms(5);
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
