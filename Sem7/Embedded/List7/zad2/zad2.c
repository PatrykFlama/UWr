#include <avr/io.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <util/delay.h>

#define UART_ECHO
#include "../../customlib/uart.c"
#include "i2c.h"

const uint8_t eeprom_addr = 0x50;

void eeprom_read_sequential(uint16_t start_addr, uint8_t* buffer, uint16_t length) {
    // random read
    uint8_t block = (start_addr >> 8) & 0x01;
    uint8_t addr = eeprom_addr | (block << 1);

    // dummy write
    i2cStart();
    i2cSend(addr << 1);
    i2cSend((uint8_t)(start_addr & 0xFF));

    // sequential read
    i2cStart();
    i2cSend((addr << 1) | 0x01);

    for (uint16_t i = 0; i < length; i++) {
        uint16_t curr_addr = start_addr + i;

        // block end (256)
        if (i > 0 && (curr_addr & 0xFF) == 0) {
            i2cStop();
            // new block
            block = (curr_addr >> 8) & 0x01;
            addr = eeprom_addr | (block << 1);

            i2cStart();
            i2cSend(addr << 1);
            i2cSend((uint8_t)(curr_addr & 0xFF));

            i2cStart();
            i2cSend((addr << 1) | 0x01);
        }

        // noack if last byte or edn of block
        if (i < length - 1 && ((curr_addr + 1) & 0xFF) != 0) {
            buffer[i] = i2cReadAck();
        } else {
            buffer[i] = i2cReadNoAck();
        }
    }

    i2cStop();
}

void eeprom_write_page(uint16_t start_addr, uint8_t* data, uint8_t length) {
    if (length == 0 || length > 16)
        return;

    uint8_t block = (start_addr >> 8) & 0x01;
    uint8_t addr = eeprom_addr | (block << 1);

    i2cStart();
    i2cSend(addr << 1);
    i2cSend((uint8_t)(start_addr & 0xFF));

    for (uint8_t i = 0; i < length; i++) {
        i2cSend(data[i]);
    }

    i2cStop();
    _delay_ms(5);
}

uint8_t hex_to_byte(char c) {
    if (c >= 'a' && c <= 'f')
        return c - 'a' + 10;
    if (c >= 'A' && c <= 'F')
        return c - 'A' + 10;
    if (c >= '0' && c <= '9')
        return c - '0';
    return 0;
}

// :LLAAAATTDD...DDCC
// LL = length, AAAA = address, TT = type, DD = data, CC = checksum
int eeprom_write_line(char* line) {
    if (line[0] != ':')
        return -1;

    const uint8_t len = (hex_to_byte(line[1]) << 4) | hex_to_byte(line[2]);

    const uint16_t addr = (hex_to_byte(line[3]) << 12) | (hex_to_byte(line[4]) << 8) |
                          (hex_to_byte(line[5]) << 4) | hex_to_byte(line[6]);

    if (512 <= addr || 512 < addr + len) {
        printf("Out of range (0-511)\r\n");
        return -1;
    }

    uint8_t data[16];

    for (uint8_t ptr = 0; ptr < len;) {
        uint16_t curr_addr = addr + ptr;
        const uint8_t pages_left = 16 - (curr_addr % 16);
        uint8_t write_len = (len - ptr) < pages_left ? (len - ptr) : pages_left;

        for (uint8_t i = 0; i < write_len; i++) {
            uint8_t pos = 9 + (ptr + i) * 2;
            data[i] = (hex_to_byte(line[pos]) << 4) | hex_to_byte(line[pos + 1]);
        }

        eeprom_write_page(curr_addr, data, write_len);

        ptr += write_len;
    }

    return len;
}

void print_hex_line(uint16_t addr, uint8_t* data, uint8_t len) {
    uint8_t checksum = len + (addr >> 8) + (addr & 0xFF) + 0x00;

    printf(":%02X%04X%02X", len, addr, 0x00);

    for (uint8_t i = 0; i < len; i++) {
        printf("%02X", data[i]);
        checksum += data[i];
    }

    checksum = (~checksum + 1) & 0xFF;
    printf("%02X\r\n", checksum);
}

int main() {
    char cmd[64];
    uint16_t addr, length;

    i2cInit();
    uart_init();

    while (1) {
        printf("> ");
        scanf("%63s", cmd);

        if (strcmp(cmd, "read") == 0) {
            scanf("%u %u", &addr, &length);

            if (512 <= addr || 512 < addr + length) {
                printf("Out of range (0-511)\r\n");
                continue;
            }

            uint8_t buffer[16];
            uint16_t remaining = length;
            uint16_t curr_addr = addr;

            while (remaining > 0) {
                uint8_t to_read = (remaining > 16) ? 16 : remaining;
                eeprom_read_sequential(curr_addr, buffer, to_read);
                print_hex_line(curr_addr, buffer, to_read);

                curr_addr += to_read;
                remaining -= to_read;
            }
        } else if (strcmp(cmd, "write") == 0) {
            char line[124];
            char c;
            uint8_t idx = 0;

            while ((c = getchar()) == ' ' || c == '\t');

            while (c != '\n' && c != '\r' && idx < 123) {
                line[idx++] = c;
                c = getchar();
            }
            line[idx] = '\0';

            int result = eeprom_write_line(line);
            if (result >= 0) {
                printf("Saved %d bytes\r\n", result);
            } else {
                printf("Incorrect i8hex line\r\n");
            }
        } else {
            printf("unknown command: %s\r\n", cmd);
            printf("commands: read <addr> <length>\twrite <hex_line>\r\n");
        }
    }
}
