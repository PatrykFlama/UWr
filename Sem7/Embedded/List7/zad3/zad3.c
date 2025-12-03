#include <avr/io.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <util/delay.h>

#include "../../customlib/uart.c"
#include "i2c.h"

const uint8_t ds_addr = 0x68;

const uint8_t REG_SECONDS = 0x00;
const uint8_t REG_MINUTES = 0x01;
const uint8_t REG_HOUR = 0x02;
const uint8_t REG_DAY = 0x03;
const uint8_t REG_DATE = 0x04;
const uint8_t REG_MONTH = 0x05;
const uint8_t REG_YEAR = 0x06;


uint8_t bcd_to_dec(uint8_t bcd) {
    return ((bcd >> 4) * 10) + (bcd & 0x0F);
}

uint8_t dec_to_bcd(uint8_t dec) {
    return ((dec / 10) << 4) | (dec % 10);
}

uint8_t read(uint8_t addr) {
    i2cStart();
    i2cSend((ds_addr << 1) | 0x00);
    i2cSend(addr);

    i2cStart();
    i2cSend((ds_addr << 1) | 0x01);
    
    uint8_t data;
    data = i2cReadNoAck();
    i2cStop();

    return data;
}

void write(uint8_t addr, uint8_t data) {
    i2cStart();
    i2cSend((ds_addr << 1) | 0x00);
    i2cSend(addr);
    i2cSend(data);
    i2cStop();
}

void read_date(uint8_t* day, uint8_t* month, uint16_t* year) {
    *day = bcd_to_dec(read(REG_DATE));
    *month = bcd_to_dec(read(REG_MONTH) & 0x1F);
    *year = 2000 + bcd_to_dec(read(REG_YEAR));
}

void read_time(uint8_t* hour, uint8_t* minutes, uint8_t* seconds) {    
    *seconds = bcd_to_dec(read(REG_SECONDS) & 0x7F);
    *minutes = bcd_to_dec(read(REG_MINUTES) & 0x7F);
    
    uint8_t bcd_hour = read(REG_HOUR);
    if (bcd_hour & 0x40) {
        uint8_t h = bcd_to_dec(bcd_hour & 0x1F);
        *hour = (bcd_hour & 0x20) ? (h % 12) + 12 : (h % 12);
    } else {
        *hour = bcd_to_dec(bcd_hour & 0x3F);
    }
}

void write_date(uint8_t day, uint8_t month, uint16_t year) {
    write(REG_DAY, dec_to_bcd(day));
    write(REG_MONTH, dec_to_bcd(month));
    write(REG_YEAR, dec_to_bcd(year - 2000));
}

void write_time(uint8_t hour, uint8_t minutes, uint8_t seconds) {
    write(REG_SECONDS, dec_to_bcd(seconds));
    write(REG_MINUTES, dec_to_bcd(minutes));
    write(REG_HOUR, dec_to_bcd(hour));
}


int main() {
    char line[64], cmd1[16], cmd2[16];

    uint8_t day, month, hour, minutes, seconds;
    uint16_t year;

    i2cInit();
    uart_init();

    while (1) {
        printf("> ");
        uart_readline(line, sizeof(line));

        if (sscanf(line, "%15s", cmd1) != 1) {
            continue;
        }

        if (strcmp(cmd1, "date") == 0) {
            read_date(&day, &month, &year);
            printf("%02u-%02u-%04u\r\n", day, month, year);
        } else if (strcmp(cmd1, "time") == 0) {
            read_time(&hour, &minutes, &seconds);
            printf("%02u:%02u:%02u\r\n", hour, minutes, seconds);
        } else if (strcmp(cmd1, "set") == 0) {
            char value[48] = {0};

            if (sscanf(line, "%15s %15s %47[^\\n]", cmd1, cmd2, value) < 2) {
                printf("unknown command\r\n");
                continue;
            }

            if (strcmp(cmd2, "date") == 0) {
                if (sscanf(value, "%hhu-%hhu-%u", &day, &month, &year) != 3) {
                    printf("Invalid format (DD-MM-YYYY)\r\n");
                    continue;
                }

                if (day < 1 || day > 31 || month < 1 || month > 12 || year < 2000 || year > 2099) {
                    printf("Out of range\r\n");
                    continue;
                }

                write_date(day, month, year);
                printf("Date set\r\n", day, month, year);
            } else if (strcmp(cmd2, "time") == 0) {
                if (sscanf(value, "%hhu:%hhu:%hhu", &hour, &minutes, &seconds) != 3) {
                    printf("Invalid format (HH:MM:SS)\r\n");
                    continue;
                }

                if (hour > 23 || minutes > 59 || seconds > 59) {
                    printf("Out of range\r\n");
                    continue;
                }

                write_time(hour, minutes, seconds);
                printf("Time set\r\n");
            } else {
                printf("unknown command\r\n");
            }
        } else {
            printf("unknown command: '%s'\r\n", line);
            printf("commands: date\t time\t set date DD-MM-YYYY\t set time HH:MM:SS\r\n");
        }
    }
}
