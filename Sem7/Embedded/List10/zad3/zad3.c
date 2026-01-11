#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "../../customlib/uart.c"

// MOSFET gate on PB5 (OC1A)
#define HEATER_DDR  DDRB
#define HEATER_PORT PORTB
#define HEATER_PIN  (1 << PB5)

// MCP9700 constants
// Vout = Tc * Ta + V0
// Ta = ambient; Tc = temperature coefficient
#define TC 0.01f    // 10 mV/C
#define V0 0.5f     // 500 mV at 0°C

typedef struct {
    int16_t target_temp;  // target temperature in 0.1°C units (e.g., 250 = 25.0°C)
    int16_t hysteresis;   // hysteresis in 0.1°C units (e.g., 10 = 1.0°C)
    uint8_t heater_on;
} controller_t;

controller_t ctrl = {300, 10, 0};

// ADC 1.1V reference
void adc_init() {
    // 1.1V internal reference and ADC0
    ADMUX = _BV(REFS1) | _BV(REFS0); // 1.1V internal ref
    DIDR0 = _BV(ADC0D); // disable digital input on ADC0
    // ADC prescaler 128 for ~125 kHz at 16 MHz
    ADCSRA = _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2) | _BV(ADEN);
}

uint16_t adc_read() {
    ADCSRA |= _BV(ADSC);
    while (!(ADCSRA & _BV(ADIF)));
    ADCSRA |= _BV(ADIF);
    return ADC;
}

int16_t adc_to_temp(uint16_t adc_val) {
    return (int16_t)(((adc_val * 1.1f / 1024.0f) - V0) / TC * 10.0f);
}

// Heater control
void heater_init() {
    HEATER_DDR |= HEATER_PIN;
    HEATER_PORT &= ~HEATER_PIN;
}

void heater_set(uint8_t on) {
    if (on) {
        HEATER_PORT |= HEATER_PIN;
    } else {
        HEATER_PORT &= ~HEATER_PIN;
    }
    ctrl.heater_on = on;
}

void controller_update(int16_t current_temp) {
    if (ctrl.heater_on) {
        if (current_temp >= ctrl.target_temp) {
            heater_set(0);
        }
    } else {
        if (current_temp < (ctrl.target_temp - ctrl.hysteresis)) {
            heater_set(1);
        }
    }
}

void command_handler() {
    printf("Commands:\n\r");
    printf("  T<value> - Set target temp (e.g., T25 for 25°C)\n\r");
    printf("  H<value> - Set hysteresis (e.g., H1 for 1°C)\n\r");

    char line[32];
    uart_readline(line, sizeof(line));

    if (strlen(line) == 0) return;

    char cmd = line[0];
    switch (cmd) {
        case 'T':
        case 't': {
            int val = atoi(&line[1]);
            ctrl.target_temp = val * 10; // Convert to 0.1°C units
            printf("Target temp set to %d.%d°C\n\r", val, 0);
            break;
        }
        case 'H':
        case 'h': {
            int val = atoi(&line[1]);
            ctrl.hysteresis = val * 10; // Convert to 0.1°C units
            printf("Hysteresis set to %d.%d°C\n\r", val, 0);
            break;
        }
        default:
            printf("Unknown command. Type '?' for help.\n\r");
    }
}

int main() {
    uart_init();
    adc_init();
    heater_init();

    printf("Temperature controller initialized\n\r");
    printf("Type '?' for help\n\r");

    uint16_t loop_count = 0;

    while (1) {
        uint16_t adc_val = adc_read();
        int16_t temp_deciC = adc_to_temp(adc_val);

        controller_update(temp_deciC);

        if (loop_count % 100 == 0) {
            int int_part = temp_deciC / 10;
            int frac_part = temp_deciC % 10;
            if (frac_part < 0) frac_part = -frac_part;

            printf("Temp: %d.%d°C Target: %d.%d°C Heater: %s\r",
                   int_part, frac_part,
                   ctrl.target_temp / 10, ctrl.target_temp % 10,
                   ctrl.heater_on ? "ON " : "OFF");
            fflush(stdout);
        }

        if (UCSR0A & _BV(RXC0)) {
            putchar('\n\r');
            command_handler();
        }

        loop_count++;
        _delay_ms(10);
    }
}
