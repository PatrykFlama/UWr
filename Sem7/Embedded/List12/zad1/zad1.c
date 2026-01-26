#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "../../customlib/uart.c"
#include "../AVR221/IAR/pid.h"

// MOSFET gate on PC1
#define HEATER_DDR  DDRC
#define HEATER_PORT PORTC
#define HEATER_PIN  (1 << PC1)

// PWM on Timer0, OC0A (PD6)
#define PWM_DDR  DDRD
#define PWM_PORT PORTD
#define PWM_PIN  (1 << PD6)

// MCP9700 constants
// Vout = Tc * Ta + V0
// Ta = ambient; Tc = temperature coefficient
#define TC 0.01f    // 10 mV/C
#define V0 0.5f     // 500 mV at 0°C

// Initial PID parameters (will be fine-tuned)
// These are scaled by SCALING_FACTOR (128)
#define PID_P   80   // Proportional
#define PID_I   20   // Integral
#define PID_D   30   // Derivative

typedef struct {
    int16_t target_temp;    // target temperature in 0.1°C units
    pidData_t pid_data;
    uint16_t pwm_duty;
} controller_t;

controller_t ctrl = {300, {0}, 0};

// ============================================================================
// ADC Functions
// ============================================================================

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

// ============================================================================
// PWM Control Functions
// ============================================================================

// PWM control for heater on Timer0, OC0A (PD6)
// Using Fast PWM mode, ~976 Hz at 16 MHz with prescaler 64
void pwm_init() {
    PWM_DDR |= PWM_PIN;
    
    // Fast PWM mode, prescaler 64
    // WGM0 = 011 (Fast PWM)
    // COM0A = 10 (non-inverting)
    // CS0 = 011 (prescaler 64)
    TCCR0A = _BV(COM0A1) | _BV(WGM01) | _BV(WGM00);
    TCCR0B = _BV(CS01) | _BV(CS00);
    
    // Start with PWM duty = 0
    OCR0A = 0;
}

void pwm_set(uint16_t duty) {
    // Clamp duty to 0-255 (8-bit)
    if (duty > 255) duty = 255;
    if (duty < 0) duty = 0;
    OCR0A = (uint8_t)duty;
    ctrl.pwm_duty = duty;
}

// ============================================================================
// Temperature Controller Functions
// ============================================================================

int16_t controller_update(int16_t current_temp) {
    int16_t pid_output = pid_Controller(ctrl.target_temp, current_temp, &ctrl.pid_data);
    
    // Clamp PID output to PWM range (0-255)
    if (pid_output > 255) pid_output = 255;
    if (pid_output < 0) pid_output = 0;
    
    pwm_set(pid_output);
    return pid_output;
}

void command_handler() {
    printf("\n\rCommands:\n\r");
    printf("  T<value> - Set target temp (e.g., T25 for 25.0°C)\n\r");
    printf("  P<value> - Set P factor (e.g., P80)\n\r");
    printf("  I<value> - Set I factor (e.g., I20)\n\r");
    printf("  D<value> - Set D factor (e.g., D30)\n\r");
    printf("  R        - Reset PID integrator\n\r");

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
        case 'P':
        case 'p': {
            int val = atoi(&line[1]);
            ctrl.pid_data.P_Factor = val;
            ctrl.pid_data.maxError = INT16_MAX / (val + 1);
            printf("P factor set to %d\n\r", val);
            break;
        }
        case 'I':
        case 'i': {
            int val = atoi(&line[1]);
            ctrl.pid_data.I_Factor = val;
            ctrl.pid_data.maxSumError = (INT32_MAX / 2) / (val + 1);
            printf("I factor set to %d\n\r", val);
            break;
        }
        case 'D':
        case 'd': {
            int val = atoi(&line[1]);
            ctrl.pid_data.D_Factor = val;
            printf("D factor set to %d\n\r", val);
            break;
        }
        case 'R':
        case 'r': {
            pid_Reset_Integrator(&ctrl.pid_data);
            printf("PID integrator reset\n\r");
            break;
        }
        default:
            printf("Unknown command. Send any character for help.\n\r");
    }
}

// ============================================================================
// Main
// ============================================================================

int main() {
    uart_init();
    adc_init();
    pwm_init();

    // Initialize PID controller
    pid_Init(PID_P, PID_I, PID_D, &ctrl.pid_data);

    printf("\n\r=== PID Temperature Controller ===\n\r");
    printf("Send any character for help\n\r");

    uint16_t loop_count = 0;

    while (1) {
        uint16_t adc_val = adc_read();
        int16_t temp_deciC = adc_to_temp(adc_val);

        controller_update(temp_deciC);

        if (loop_count % 100 == 0) {
            int int_part = temp_deciC / 10;
            int frac_part = temp_deciC % 10;
            if (frac_part < 0) frac_part = -frac_part;

            int target_int = ctrl.target_temp / 10;
            int target_frac = ctrl.target_temp % 10;

            printf("Temp: %d.%d°C Target: %d.%d°C PWM: %3d Error: %d.%d°C\r",
                   int_part, frac_part,
                   target_int, target_frac,
                   ctrl.pwm_duty,
                   (temp_deciC - ctrl.target_temp) / 10,
                   abs((temp_deciC - ctrl.target_temp) % 10));
            fflush(stdout);
        }

        if (UCSR0A & _BV(RXC0)) {
            command_handler();
        }

        loop_count++;
        _delay_ms(10);  // 100 Hz control loop
    }
}
