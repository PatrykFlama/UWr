#include <avr/io.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <util/delay.h>

#define UART_ECHO
#include "../../customlib/uart.c"

#include "i2c.h"

#define EEPROM_ADDR 0x50
#define PAGE_SIZE 16    // bajty

void eeprom_read_sequential(uint16_t start_addr, uint8_t* buffer, uint16_t length) {
    // po odczycie bajtu, wysłanie ACK inkrementuje wewnętrzny licznik adresu i kontynuuje odczyt

    // random read
    uint8_t block = (start_addr >> 8) & 0x01;
    uint8_t device_addr = EEPROM_ADDR | (block << 1);

    // Dummy write - ustawienie początkowego adresu
    i2cStart();
    i2cSend(device_addr << 1);
    i2cSend((uint8_t)(start_addr & 0xFF));

    // sekwencyjny odczyt
    i2cStart();
    i2cSend((device_addr << 1) | 0x01);

    // kolejne bajty
    for (uint16_t i = 0; i < length; i++) {
        uint16_t current_addr = start_addr + i;

        // Sprawdź czy przekroczyliśmy granicę bloku (256 bajtów)
        // Jeśli tak, musimy rozpocząć nową transakcję
        if (i > 0 && (current_addr & 0xFF) == 0) {
            i2cStop();

            // Nowy blok - rozpocznij od nowa
            block = (current_addr >> 8) & 0x01;
            device_addr = EEPROM_ADDR | (block << 1);

            i2cStart();
            i2cSend(device_addr << 1);
            i2cSend((uint8_t)(current_addr & 0xFF));

            i2cStart();
            i2cSend((device_addr << 1) | 0x01);
        }

        // odczytaj bajt, NACK jest wysyłany po ostatnim bajcie lub przy granicy bloku
        if (i < length - 1 && ((current_addr + 1) & 0xFF) != 0) {
            buffer[i] = i2cReadAck();
        } else {
            buffer[i] = i2cReadNoAck();
        }
    }

    i2cStop();
}

// Zapis stronami (Page Write)
// 24C04 ma strony 16-bajtowe. Jeśli dane przekroczą granicę strony,
// adres "zawija się" do początku tej samej strony
void eeprom_write_page(uint16_t start_addr, uint8_t* data, uint8_t length) {
    if (length == 0 || length > PAGE_SIZE)
        return;

    uint8_t block = (start_addr >> 8) & 0x01;
    uint8_t device_addr = EEPROM_ADDR | (block << 1);

    i2cStart();
    i2cSend(device_addr << 1);
    i2cSend((uint8_t)(start_addr & 0xFF));

    // Wyślij wszystkie bajty
    for (uint8_t i = 0; i < length; i++) {
        i2cSend(data[i]);
    }

    i2cStop();
    _delay_ms(10);  // Wait for write cycle
}

uint8_t hex_to_byte(char c) {
    if (c >= '0' && c <= '9')
        return c - '0';
    if (c >= 'A' && c <= 'F')
        return c - 'A' + 10;
    if (c >= 'a' && c <= 'f')
        return c - 'a' + 10;
    return 0;
}

// Parsowanie linii Intel HEX i zapis do EEPROM
// Format: :LLAAAATTDD...DDCC
// LL = length, AAAA = address, TT = type, DD = data, CC = checksum
int parse_hex_line(char* line) {
    if (line[0] != ':')
        return -1;

    // Parsuj długość
    uint8_t len = (hex_to_byte(line[1]) << 4) | hex_to_byte(line[2]);

    // Parsuj adres
    uint16_t addr = (hex_to_byte(line[3]) << 12) | (hex_to_byte(line[4]) << 8) |
                    (hex_to_byte(line[5]) << 4) | hex_to_byte(line[6]);

    // Parsuj typ rekordu
    uint8_t type = (hex_to_byte(line[7]) << 4) | hex_to_byte(line[8]);

    if (type != 0x00)
        return 0;  // Tylko data records

    // Sprawdź czy adres mieści się w zakresie
    if (addr + len > 512) {
        printf("Error: Address out of range\r\n");
        return -1;
    }

    // Parsuj dane i zapisz do EEPROM stronami
    uint8_t data[16];
    uint8_t written = 0;

    while (written < len) {
        uint16_t current_addr = addr + written;
        uint8_t page_offset = current_addr % PAGE_SIZE;
        uint8_t page_remaining = PAGE_SIZE - page_offset;
        uint8_t to_write = (len - written) < page_remaining ? (len - written) : page_remaining;

        // Parsuj dane dla tej strony
        for (uint8_t i = 0; i < to_write; i++) {
            uint8_t pos = 9 + (written + i) * 2;
            data[i] = (hex_to_byte(line[pos]) << 4) | hex_to_byte(line[pos + 1]);
        }

        eeprom_write_page(current_addr, data, to_write);
        written += to_write;
    }

    return len;
}

// dane w formacie Intel HEX
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
    char cmd[20];
    uint16_t addr, length;

    i2cInit();
    uart_init();

    while (1) {
        printf("> ");
        scanf("%s", cmd);

        if (strcmp(cmd, "read") == 0) {
            scanf("%u %u", &addr, &length);

            if (addr > 511 || addr + length > 512) {
                printf("Error: Address out of range (0-511)\r\n");
                continue;
            }

            uint8_t buffer[16];
            uint16_t remaining = length;
            uint16_t current_addr = addr;

            while (remaining > 0) {
                uint8_t to_read = (remaining > 16) ? 16 : remaining;
                eeprom_read_sequential(current_addr, buffer, to_read);
                print_hex_line(current_addr, buffer, to_read);

                current_addr += to_read;
                remaining -= to_read;
            }
        } else if (strcmp(cmd, "write") == 0) {
            // reszta linii (linia HEX)
            char hex_line[100];
            char c;
            uint8_t idx = 0;

            // pomiń białe znaki
            while ((c = getchar()) == ' ' || c == '\t');

            // wczytaj całą linię
            do {
                hex_line[idx++] = c;
                c = getchar();
            } while (c != '\n' && c != '\r' && idx < 99);
            hex_line[idx] = '\0';

            int result = parse_hex_line(hex_line);
            if (result >= 0) {
                printf("Wrote %d bytes\r\n", result);
            } else {
                printf("Error parsing HEX line\r\n");
            }
        } else {
            printf("unknown command: %s\r\n", cmd);
            printf("commands: read <addr> <length>\twrite <hex_line>\r\n");
        }
    }
}
