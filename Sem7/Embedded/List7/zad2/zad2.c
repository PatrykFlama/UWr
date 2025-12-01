#include <avr/io.h>
#include <inttypes.h>
#include <stdio.h>
#include <util/delay.h>

#include "i2c.h"

#include "../../customlib/uart.c"

#include <string.h>
#include <stdlib.h>


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

        // read line from UART (use local readline with echo)
        uart_readline(line, sizeof(line));

        // parse commands
        unsigned int addr = 0, value = 0, length = 0;
        // read addr length -> perform sequential read and print Intel HEX records
        if (sscanf(line, "read %i %i", &addr, &length) == 2) {
            if (length == 0) {
                printf(":00000001FF\r\n"); // EOF record
                continue;
            }
            if (addr > 0x1FF) {
                printf("addr out of range (0..0x1FF)\r\n");
                continue;
            }
            // read `length` bytes starting at addr
            uint32_t remaining = length;
            uint16_t cur = (uint16_t)addr;
            // We'll read in chunks but minimize I2C transactions: perform one sequential read for the whole length
            // Set address
            i2cStart();
            if ((TWSR & 0xF8) != 0x08) { i2cStop(); printf("I2C start failed\r\n"); continue; }
            i2cSend(eeprom_addr | ((cur & 0x100) >> 7));
            if ((TWSR & 0xF8) != 0x18) { i2cStop(); printf("I2C write addr failed\r\n"); continue; }
            i2cSend(cur & 0xFF);
            if ((TWSR & 0xF8) != 0x28) { i2cStop(); printf("I2C set mem addr failed\r\n"); continue; }
            // repeated start for read
            i2cStart();
            if ((TWSR & 0xF8) != 0x10) { i2cStop(); printf("I2C restart failed\r\n"); continue; }
            i2cSend(eeprom_addr | 0x1 | ((cur & 0x100) >> 7));
            if ((TWSR & 0xF8) != 0x40) { i2cStop(); printf("I2C read request failed\r\n"); continue; }
            // read bytes sequentially
            uint8_t buf[32];
            while (remaining) {
                uint8_t toread = remaining > sizeof(buf) ? sizeof(buf) : (uint8_t)remaining;
                for (uint8_t i = 0; i < toread; ++i) {
                    uint8_t v;
                    if (remaining == 1) v = i2cReadNoAck(); else v = i2cReadAck();
                    buf[i] = v;
                    remaining--;
                }
                // print buffered data as Intel HEX records of up to 16 bytes per line
                uint8_t *p = buf;
                uint8_t printed = 0;
                while (printed < toread) {
                    uint8_t rec_len = (toread - printed) > 16 ? 16 : (toread - printed);
                    uint16_t rec_addr = cur;
                    // compute checksum
                    uint8_t sum = 0;
                    sum += rec_len;
                    sum += (rec_addr >> 8) & 0xFF;
                    sum += rec_addr & 0xFF;
                    sum += 0x00; // record type
                    // print data
                    printf(":" );
                    printf("%02X%04X00", rec_len, rec_addr);
                    for (uint8_t j = 0; j < rec_len; ++j) {
                        uint8_t d = p[printed + j];
                        sum += d;
                        printf("%02X", d);
                    }
                    uint8_t chksum = ((uint8_t)(-sum));
                    printf("%02X\r\n", chksum);
                    cur += rec_len;
                    printed += rec_len;
                }
            }
            i2cStop();
            // EOF record
            printf(":00000001FF\r\n");
        }
        // write -> expect subsequent Intel HEX lines until EOF record (type 01)
        else if (strcmp(line, "write") == 0) {
            // read lines until EOF record
            int done = 0;
            while (!done) {
                uart_readline(line, sizeof(line));
                size_t l = strlen(line);
                if (l == 0) continue;
                if (line[0] != ':') { printf("bad hex line: %s\r\n", line); continue; }
                // parse hex fields
                // byte count
                char *p = line + 1;
                unsigned int bytecount = (unsigned int)strtoul(p, NULL, 16);
                // address
                p += 2;
                unsigned int rec_addr = (unsigned int)strtoul(p, NULL, 16);
                p += 4;
                unsigned int rectype = (unsigned int)strtoul(p, NULL, 16);
                p += 2;
                // data
                uint8_t data[256];
                for (unsigned int i = 0; i < bytecount; ++i) {
                    data[i] = (uint8_t)strtoul(p + i*2, NULL, 16);
                }
                // checksum not validated strictly here
                if (rectype == 0x01) {
                    done = 1;
                    break;
                }
                if (rectype != 0x00) continue;
                // write data[] of length bytecount starting at rec_addr
                unsigned int offset = 0;
                while (offset < bytecount) {
                    uint16_t a = (uint16_t)(rec_addr + offset);
                    uint8_t page_off = a & 0x0F;
                    uint8_t space = 16 - page_off;
                    uint8_t chunk = (bytecount - offset) < space ? (bytecount - offset) : space;
                    // start I2C write for this page chunk
                    i2cStart();
                    if ((TWSR & 0xF8) != 0x08) { i2cStop(); printf("I2C start failed during write\r\n"); break; }
                    i2cSend(eeprom_addr | ((a & 0x100) >> 7));
                    if ((TWSR & 0xF8) != 0x18) { i2cStop(); printf("I2C device addr failed during write\r\n"); break; }
                    i2cSend(a & 0xFF);
                    if ((TWSR & 0xF8) != 0x28) { i2cStop(); printf("I2C mem addr failed during write\r\n"); break; }
                    for (uint8_t k = 0; k < chunk; ++k) {
                        i2cSend(data[offset + k]);
                        if ((TWSR & 0xF8) != 0x28) {
                            // continue sending but warn
                        }
                    }
                    i2cStop();
                    _delay_ms(6); // allow write cycle (<=5ms per datasheet)
                    offset += chunk;
                }
            }
            printf("write completed\r\n");
        }
        // single byte read/write from zad1-compatible commands
        else if (sscanf(line, "read %i", &addr) == 1) {
            if (addr > 0x1FF) { printf("addr out of range (0..0x1FF)\r\n"); continue; }
            uint8_t v;
            if (eeprom_read_byte((uint16_t)addr, &v) == 0) printf("0x%03x: 0x%02x\r\n", addr, v); else printf("read failed at 0x%03x\r\n", addr);
        } else if (sscanf(line, "write %i %i", &addr, &value) == 2) {
            if (addr > 0x1FF) { printf("addr out of range (0..0x1FF)\r\n"); continue; }
            if (value > 0xFF) { printf("value out of range (0..0xFF)\r\n"); continue; }
            if (eeprom_write_byte((uint16_t)addr, (uint8_t)value) == 0) printf("wrote 0x%02x to 0x%03x\r\n", value, addr); else printf("write failed at 0x%03x\r\n", addr);
        } else if (line[0] == '\0') {
            // empty line, ignore
        } else {
            printf("unknown command: '%s'\r\n", line);
            printf("commands: read addr length\tread addr\twrite (then i8hex lines)\r\n");
        }
    }
}
