#include <avr/io.h>
#include <inttypes.h>
#include <stdio.h>
#include <util/delay.h>
#include <string.h>
#include <stdlib.h>

#include "i2c.h"

#include "../../customlib/uart.c"

const uint8_t ds_addr = 0x68;

#define REG_SECONDS 0x00
#define REG_MINUTES 0x01
#define REG_HOURS 0x02
#define REG_DAY 0x03
#define REG_DATE 0x04
#define REG_MONTH 0x05
#define REG_YEAR 0x06

uint8_t bcd_to_dec(uint8_t bcd) {
    return ((bcd >> 4) * 10) + (bcd & 0x0F);
}

uint8_t dec_to_bcd(uint8_t dec) {
    return ((dec / 10) << 4) | (dec % 10);
}

uint8_t read_register(uint8_t reg_addr) {
    uint8_t data;
    i2cStart();
    i2cSend((ds_addr << 1) | 0x00);  // write
    i2cSend(reg_addr);
    i2cStart();
    i2cSend((ds_addr << 1) | 0x01);  // read
    data = i2cReadNoAck();
    i2cStop();
    return data;
}

void write_register(uint8_t reg_addr, uint8_t data) {
    i2cStart();
    i2cSend((ds_addr << 1) | 0x00);
    i2cSend(reg_addr);
    i2cSend(data);
    i2cStop();
}

void get_date(uint8_t* day, uint8_t* month, uint16_t* year) {
    *day = bcd_to_dec(read_register(REG_DATE));
    *month = bcd_to_dec(read_register(REG_MONTH) & 0x1F);
    *year = 2000 + bcd_to_dec(read_register(REG_YEAR));
}

void get_time(uint8_t* hours, uint8_t* minutes, uint8_t* seconds) {
    uint8_t bcd_hours = read_register(REG_HOURS);
    uint8_t bcd_minutes = read_register(REG_MINUTES);
    uint8_t bcd_seconds = read_register(REG_SECONDS);
    *seconds = bcd_to_dec(bcd_seconds & 0x7F);
    *minutes = bcd_to_dec(bcd_minutes & 0x7F);
    if (bcd_hours & 0x40) {
        // 12-hour mode
        *hours = bcd_to_dec(bcd_hours & 0x1F);
        if (bcd_hours & 0x20)
            *hours += 12;
    } else {
        *hours = bcd_to_dec(bcd_hours & 0x3F);
    }
}

void set_date(uint8_t day, uint8_t month, uint16_t year) {
    write_register(REG_DATE, dec_to_bcd(day));
    write_register(REG_MONTH, dec_to_bcd(month));
    write_register(REG_YEAR, dec_to_bcd(year - 2000));
}

void set_time(uint8_t hours, uint8_t minutes, uint8_t seconds) {
    write_register(REG_SECONDS, dec_to_bcd(seconds));
    write_register(REG_MINUTES, dec_to_bcd(minutes));
    write_register(REG_HOURS, dec_to_bcd(hours));
}

void flush_input_buffer(void) {
    int c;
    while ((c = getchar()) != '\n' && c != '\r' && c != EOF);
}

int main() {
    char line[64];
    char cmd[16];
    char subcmd[16];
    uint8_t day, month, hours, minutes, seconds;
    unsigned int year;

    i2cInit();
    uart_init();

    while (1) {
        printf("> ");
        uart_readline(line, sizeof(line));

        if (sscanf(line, "%15s", cmd) != 1) {
            continue;
        }

        if (strcmp(cmd, "date") == 0) {
            get_date(&day, &month, &year);
            printf("%02u-%02u-%04u\r\n", day, month, year);
        } else if (strcmp(cmd, "time") == 0) {
            get_time(&hours, &minutes, &seconds);
            printf("%02u:%02u:%02u\r\n", hours, minutes, seconds);
        } else if (strcmp(cmd, "set") == 0) {
            char rest[48] = {0};
            if (sscanf(line, "%15s %15s %47[^\\n]", cmd, subcmd, rest) < 2) {
                printf("Error: Missing subcommand\r\n");
                continue;
            }

            if (strcmp(subcmd, "date") == 0) {
                if (sscanf(rest, "%hhu-%hhu-%u", &day, &month, &year) != 3) {
                    printf("Error: Invalid date format (expected DD-MM-YYYY)\r\n");
                    continue;
                }
                if (day < 1 || day > 31 || month < 1 || month > 12 || year < 2000 || year > 2099) {
                    printf("Error: Date out of range\r\n");
                    continue;
                }
                set_date(day, month, year);
                printf("Date set to: %02u-%02u-%04u\r\n", day, month, year);
            } else if (strcmp(subcmd, "time") == 0) {
                if (sscanf(rest, "%hhu:%hhu:%hhu", &hours, &minutes, &seconds) != 3) {
                    printf("Error: Invalid time format (expected HH:MM:SS)\r\n");
                    continue;
                }
                if (hours > 23 || minutes > 59 || seconds > 59) {
                    printf("Error: Time out of range\r\n");
                    continue;
                }
                set_time(hours, minutes, seconds);
                printf("Time set to: %02u:%02u:%02u\r\n", hours, minutes, seconds);
            } else {
                printf("Error: Unknown subcommand (use 'date' or 'time')\r\n");
            }
        } else {
            printf("unknown command: '%s'\r\n", line);
            printf("commands: date\t time\t set date DD-MM-YYYY\t set time HH:MM:SS\r\n");
        }
    }
}
